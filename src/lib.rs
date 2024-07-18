//! # Graph UI
//!
//! This allow to draw in egui a petgraph using a node formatter function (returning a String).
//!
//! One actual drawback of the implementation is the performances (we could optimize with a cache and a quadtree coordinate system)
//! and the fact that after the "layouting" we don't have the whole node but just a formatted text (i.e. in case of a network graph,
//! we cannot know if the not correspond to a compromised machine and do extra animation). A solution is to add an id in the layout node text
//! and make a function using that id to associate a rect position and text to a specific node (kind of dirty hack...).
//!
//! Main things to do:
//! - Optimization (cache + quadtree to know which element to draw if collision with the screen area),
//! - Node association to layout graph (either layout fork or id in text solution).

use eframe::egui;

use egui::epaint::PathShape;
use egui::{Color32, Context, Id, Pos2, Rect, Response, Sense, Stroke, Ui};
use egui::{Layout, Vec2};
use petgraph::stable_graph::{DefaultIx, EdgeIndex, IndexType, NodeIndex};
use petgraph::{Directed, EdgeType};
use rand::Rng;
use std::fmt::Debug;
use std::marker::PhantomData;

pub mod graph_elements;
pub mod transform;

use transform::TransformState;

use self::graph_elements::{DrawableGraph, DrawableGraphConfig};

/// Zoom step to apply.
const ZOOM_STEP: f32 = 0.002;

/// Box margin used when auto resizing the view.
const BOX_MARGIN: f32 = 20.0;

/// Possible closures to call on node drawing.
/// Will be called for each node.
#[allow(clippy::type_complexity)]
enum ShowNode<S, N, E, Ty: EdgeType = Directed, Ix: IndexType + Debug = DefaultIx> {
    /// See `Graph::show_node`.
    Simple(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32)>),
    /// See `Graph::show_node_with_state`.
    WithState(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, &mut S)>),
    /// See `Graph::show_node_interactive`.
    Interactive(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, Response)>),
    /// See `Graph::show_node_interactive_with_state`.
    InteractiveWithState(
        Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, &mut S, Response)>,
    ),
}

/// Possible closures to call on edge drawing.
/// Will be called for each edge.
#[allow(clippy::type_complexity)]
enum ShowEdge<S, N, E, Ty: EdgeType = Directed, Ix: IndexType + Debug = DefaultIx> {
    /// See `Graph::show_edge`.
    Simple(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32)>),
    /// See `Graph::show_edge_with_state`.
    WithState(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, &mut S)>),
    /// See `Graph::show_edge_interactive`.
    Interactive(Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, Response)>),
    /// See `Graph::show_edge_interactive_with_state`.
    InteractiveWithState(
        Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, &mut S, Response)>,
    ),
}

/// Store a drawable representation of a graph, after a layout pass (positioning was done).
///
/// TODO: we should add a "hook" system after layout to keep the node. To do that we know that a node text center = rect center,
/// so if we store an id in the text, we can retrieve the associated text and rect of the associated node.
#[allow(clippy::type_complexity)]
pub struct Graph<S, N, E, Ty: EdgeType = Directed, Ix: IndexType + Debug = DefaultIx> {
    /// Inner drawable graph.
    inner: DrawableGraph<N, E, Ty, Ix>,
    /// Closure to know what to draw to represents a node.
    show_node_closure: Option<ShowNode<S, N, E, Ty, Ix>>,
    /// Closure to know what to draw to represents an edge.
    show_edge_closure: Option<ShowEdge<S, N, E, Ty, Ix>>,
    /// Closure to know how to draw to represents a link.
    show_link_closure:
        Option<Box<dyn Fn(&mut petgraph::Graph<N, E, Ty, Ix>, &Context, EdgeIndex, f32) -> Stroke>>,
    /// Id of the graph.
    id: Id,
    /// Graph state (phantom data).
    phantom: PhantomData<S>,
    /// Has the graph changed or not.
    graph_need_update: bool,
    /// Maximum bound of the graph size in `x`.
    /// If the limit is hit, only the right size of the graph will be shown on `auto_resize`.
    /// The visible graph will have a virtual bounding box in `x` min of `bounding_box.x - window_size`.
    /// So in `x` only values in `[bounding_box.max.x - window_size;bounding_box.max.x]` will be shown.
    maximum_bound_x: Option<f32>,
    /// Maximum bound of the graph size in `y`.
    /// If the limit is hit, only the right size of the graph will be shown on `auto_resize`.
    /// The visible graph will have a virtual bounding box in `y` min of `bounding_box.y - window_size`.
    /// So in `y` only values in `[bounding_box.max.y - window_size;bounding_box.max.y]` will be shown.
    maximum_bound_y: Option<f32>,
    /// Margin to use when `auto_resize` (i.e. to not have extremities nodes overlapping with graph view limits).
    node_margin: Vec2,
}

impl<S, N, E, Ty: EdgeType, Ix: IndexType + Debug> Graph<S, N, E, Ty, Ix> {
    /// Create a new graph (egi drawable) from a petgraph graph.
    pub fn new() -> Self {
        Self {
            inner: DrawableGraph::new(),
            show_node_closure: None,
            show_edge_closure: None,
            show_link_closure: None,
            id: Id::new(rand::thread_rng().gen::<u64>()),
            phantom: PhantomData,
            graph_need_update: true,
            maximum_bound_x: None,
            maximum_bound_y: None,
            node_margin: Vec2::new(BOX_MARGIN, BOX_MARGIN),
        }
    }

    /// Simply show a node without state or [`egui::Response`].
    ///
    /// Helper function for underlying `ShowNode::Simple`.
    pub fn show_node<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32) + 'static,
    {
        self.show_node_closure = Some(ShowNode::Simple(Box::new(closure)));
        self
    }

    /// Show a node regarding a provided closure, providing a state (e.g. app state).
    /// If used, the state have to be providing when showing the graph (otherwise nothing will be shown).
    ///
    /// Helper function for underlying `ShowNode::WithState`.
    pub fn show_node_with_state<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, &mut S) + 'static,
    {
        self.show_node_closure = Some(ShowNode::WithState(Box::new(closure)));
        self
    }

    /// Show a node regarding a provided closure, providing an [`egui::Response`] used to check if the node is clicked
    /// or anything else.
    ///
    /// NOTE: Using this will disable drag/move tracking for graph movement/position reset (for all nodes).
    ///
    /// Helper function for underlying `ShowNode::Interactive`.
    pub fn show_node_interactive<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, Response) + 'static,
    {
        self.show_node_closure = Some(ShowNode::Interactive(Box::new(closure)));
        self
    }

    /// Show a node regarding a provided closure, providing a state (e.g. app state) and an [`egui::Response`] used to
    /// check if the node is clicked or anything else.
    ///
    /// NOTE: Using this will disable drag/move tracking for graph movement/position reset (for all nodes).
    ///
    /// Helper function for underlying `ShowNode::InteractiveWithState`.
    pub fn show_node_interactive_with_state<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, NodeIndex, &mut Ui, f32, &mut S, Response)
            + 'static,
    {
        self.show_node_closure = Some(ShowNode::InteractiveWithState(Box::new(closure)));
        self
    }

    /// Helper function for underlying `ShowEdge::Simple`.
    pub fn show_edge<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32) + 'static,
    {
        self.show_edge_closure = Some(ShowEdge::Simple(Box::new(closure)));
        self
    }

    /// Helper function for underlying `ShowEdge::WithState`.
    pub fn show_edge_with_state<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, &mut S) + 'static,
    {
        self.show_edge_closure = Some(ShowEdge::WithState(Box::new(closure)));
        self
    }

    /// Helper function for underlying `ShowEdge::Interactive`.
    pub fn show_edge_interactive<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, Response) + 'static,
    {
        self.show_edge_closure = Some(ShowEdge::Interactive(Box::new(closure)));
        self
    }

    /// Helper function for underlying `ShowEdge::InteractiveWithState`.
    pub fn show_edge_interactive_with_state<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, EdgeIndex, &mut Ui, f32, &mut S, Response)
            + 'static,
    {
        self.show_edge_closure = Some(ShowEdge::InteractiveWithState(Box::new(closure)));
        self
    }

    /// Set a closure to be called when drawing a link.
    /// This can be used to define links style.
    pub fn show_link<F>(mut self, closure: F) -> Self
    where
        F: Fn(&mut petgraph::Graph<N, E, Ty, Ix>, &Context, EdgeIndex, f32) -> Stroke + 'static,
    {
        self.show_link_closure = Some(Box::new(closure));
        self
    }

    /// Max size of a node (represents its shape).
    pub fn set_node_size(mut self, node_size: Vec2) -> Self {
        self.inner.config.node_box_size = node_size;
        self
    }

    /// Max size of an edge (represents its shape).
    pub fn set_edge_box_size(mut self, edge_box_size: Vec2) -> Self {
        self.inner.config.edge_box_size = edge_box_size;
        self
    }

    /// Default text size.
    pub fn set_text_font_size(mut self, text_font_size: f32) -> Self {
        self.inner.config.text_font_size = text_font_size;
        self
    }

    /// Sampling for bezier curves.
    /// Number of sampling to apply to draw a bezier curve. Higher it is, smoother it will be rendered.
    pub fn set_bezier_curve_sampling(mut self, bezier_curve_sampling: usize) -> Self {
        self.inner.config.bezier_curve_sampling = bezier_curve_sampling;
        self
    }

    /// Maximum bound of the graph size in `x`.
    /// If the limit is hit, only the right size of the graph will be shown on `auto_resize`.
    /// The visible graph will have a virtual bounding box in `x` min of `bounding_box.x - window_size`.
    /// So in `x` only values in `[bounding_box.max.x - window_size;bounding_box.max.x]` will be shown.
    pub fn set_maximum_bound_x(mut self, maximum_bound_x: f32) -> Self {
        self.maximum_bound_x = Some(maximum_bound_x);
        self
    }

    /// Maximum bound of the graph size in `y`.
    /// If the limit is hit, only the right size of the graph will be shown on `auto_resize`.
    /// The visible graph will have a virtual bounding box in `y` min of `bounding_box.y - window_size`.
    /// So in `y` only values in `[bounding_box.max.y - window_size;bounding_box.max.y]` will be shown.
    pub fn set_maximum_bound_y(mut self, maximum_bound_y: f32) -> Self {
        self.maximum_bound_y = Some(maximum_bound_y);
        self
    }

    /// Margin to use when `auto_resize` (i.e. to not have extremities nodes overlapping with graph view limits).
    pub fn set_node_margin(mut self, node_margin: Vec2) -> Self {
        self.node_margin = node_margin;
        self
    }

    /// Set the graph to use (initial graph).
    pub fn set_graph(mut self, graph: petgraph::Graph<N, E, Ty, Ix>) -> Self {
        self.inner.set_graph(graph);
        self.graph_need_update = true;
        self
    }

    /// Set a new drawable `GraphUi` from a `petgraph` (keeping all ids and transformation).
    /// Helpful to keep IDs between replacement (e.g. keep same zoom/movement).
    ///
    /// # Arguments
    ///
    /// `graph` - The petgraph to convert.
    pub fn update_graph(&mut self, graph: petgraph::Graph<N, E, Ty, Ix>) {
        self.inner.set_graph(graph);
        self.graph_need_update = true;
    }

    /// Get inner graph.
    pub fn graph(&self) -> Option<&petgraph::Graph<N, E, Ty, Ix>> {
        self.inner.inner.as_ref()
    }

    /// Get inner graph mutably.
    pub fn graph_mut(&mut self) -> Option<&mut petgraph::Graph<N, E, Ty, Ix>> {
        self.inner.inner.as_mut()
    }

    /// Show/paint the graph with transformations.
    /// Also wait and apply user interactions (drag, zoom...).
    ///
    /// **Warning: calling this function will convert and compute the graph layout (if needed, i.e. if graph changed), so this is compute intensive.**
    pub fn show(&mut self, ui: &mut Ui, state: Option<&mut S>) {
        // If the graph has changed since last show, we need to update it
        if self.graph_need_update {
            self.inner.update();
            self.graph_need_update = false;
        }

        // If there is nothing to draw, just skip everything
        if self.inner.nodes.is_empty() && self.inner.edges.is_empty() {
            return;
        }

        // Create a new transform or load the old one
        let mut transform = TransformState::load_from_ui_memory(ui, self.id.with("zoom"))
            .unwrap_or_else(|| TransformState::new(self.id.with("zoom"), ui));

        // Upload in transform the rect drawable
        transform.draw_rect = ui.available_rect_before_wrap();

        // Recovers the interactions
        let response = ui.interact(transform.draw_rect, self.id, Sense::click_and_drag());

        // Paint the graph with zoom, drag
        transform.show(ui, |ui| {
            self.draw(ui, &transform, state);
        });

        // Handle drag.
        if response.dragged() {
            transform.user_modified = true;
            transform.drag(response.drag_delta());
        }

        // Handle zoom
        if let Some(pos) = ui.ctx().pointer_latest_pos() {
            let zoom = ui.input(|i| i.scroll_delta.y);
            if zoom != 0. && transform.draw_rect.contains(pos) {
                transform.user_modified = true;
                let zoom = (zoom * ZOOM_STEP).exp();
                transform.zoom(pos, zoom);
            }
        }

        // Handle reset double click
        if response.double_clicked() || !transform.user_modified {
            self.auto_resize(ui, &mut transform);
        }

        // Save the transform into egui memory
        transform.store_in_ui_memory(ui);
    }

    /// Draw the graph.
    ///
    /// Maybe we could optimize this function since we'll iter on all element of the graph and draw them.
    /// We doesn't cache anything so we recalcultate all at each call.
    ///
    /// **WARNING: this is not optimized at all**
    ///
    /// TODO: this could be optimized by storing Shapes and reuse them if the user didn't zoom/drag.
    fn draw(&mut self, ui: &mut Ui, transform: &TransformState, mut state: Option<&mut S>) {
        let Some(mut graph) = self.inner.inner.take() else {
            return;
        };

        // Render everything
        // NOTE: order is very important here, it's the same as the draw order
        self.draw_lines(ui, transform, &mut graph);
        self.draw_edges(ui, transform, &mut state, &mut graph);
        self.draw_nodes(ui, transform, &mut state, &mut graph);

        self.inner.inner = Some(graph);
    }

    /// Draw lines.
    fn draw_lines(
        &mut self,
        ui: &mut Ui,
        transform: &TransformState,
        graph: &mut petgraph::Graph<N, E, Ty, Ix>,
    ) {
        for (index, edge) in &self.inner.lines {
            // Arrow default color
            let color = Color32::WHITE;

            // Line stroke
            let connection_stroke = if let Some(connection_stroke) = &mut self.show_link_closure {
                (connection_stroke)(
                    graph,
                    ui.ctx(),
                    EdgeIndex::new(*index),
                    transform.zoom_factor,
                )
            } else {
                Stroke {
                    width: 1.0 * transform.zoom_factor,
                    color,
                }
            };

            // Show edges link bezier curves
            let bezier = PathShape::line(
                edge.sampled_lines()
                    .iter()
                    .map(|position| transform.graph_pos_to_screen_pos(*position))
                    .collect(),
                connection_stroke,
            );

            ui.painter().add(bezier);
        }
    }

    /// Draw edges.
    fn draw_edges(
        &mut self,
        ui: &mut Ui,
        transform: &TransformState,
        state: &mut Option<&mut S>,
        graph: &mut petgraph::Graph<N, E, Ty, Ix>,
    ) {
        if let Some(show_edge) = &self.show_edge_closure {
            for (index, edge) in &self.inner.edges {
                let rect = Rect::from_center_size(
                    transform.graph_pos_to_screen_pos(edge.content_position),
                    Vec2::new(
                        self.inner.config.edge_box_size.x * transform.zoom_factor,
                        self.inner.config.edge_box_size.y * transform.zoom_factor,
                    ),
                );

                // Here we draw the edge content (can be any widget)
                let mut child_ui = ui.child_ui(rect, Layout::top_down(egui::Align::Center));

                // Regarding show edge value (which closure parameter to provide), call underlying closure
                Self::show_transformed(&mut child_ui, transform, &self.inner.config, |ui| {
                    match show_edge {
                        ShowEdge::Simple(show_edge) => {
                            // Call show edge closure made by the user (telling us what to draw in place of the edge)
                            (show_edge)(graph, EdgeIndex::new(*index), ui, transform.zoom_factor);
                        }
                        ShowEdge::WithState(show_edge) => {
                            if let Some(state) = state {
                                // Call show edge closure made by the user (telling us what to draw in place of the edge)
                                (show_edge)(
                                    graph,
                                    EdgeIndex::new(*index),
                                    ui,
                                    transform.zoom_factor,
                                    state,
                                );
                            }
                        }
                        ShowEdge::Interactive(show_edge) => {
                            // Recovers the interactions
                            // NOTE: here it's important to have an id PER edge
                            // NOTE: this make the node region loosing drag and double click capability (graph reposition/move)
                            let response = ui.interact(
                                rect,
                                self.id.with("edges").with(index),
                                Sense::click(),
                            );

                            // Call show edge closure made by the user (telling us what to draw in place of the edge)
                            (show_edge)(
                                graph,
                                EdgeIndex::new(*index),
                                ui,
                                transform.zoom_factor,
                                response,
                            );
                        }
                        ShowEdge::InteractiveWithState(show_edge) => {
                            if let Some(state) = state {
                                // Recovers the interactions
                                // NOTE: here it's important to have an id PER edge
                                // NOTE: this make the node region loosing drag and double click capability (graph reposition/move)
                                let response = ui.interact(
                                    rect,
                                    self.id.with("edges").with(index),
                                    Sense::click(),
                                );

                                // Call show edge closure made by the user (telling us what to draw in place of the edge)
                                (show_edge)(
                                    graph,
                                    EdgeIndex::new(*index),
                                    ui,
                                    transform.zoom_factor,
                                    state,
                                    response,
                                );
                            }
                        }
                    }
                });
            }
        }
    }

    /// Draw nodes.
    fn draw_nodes(
        &mut self,
        ui: &mut Ui,
        transform: &TransformState,
        state: &mut Option<&mut S>,
        graph: &mut petgraph::Graph<N, E, Ty, Ix>,
    ) {
        if let Some(show_node) = &self.show_node_closure {
            for (index, node) in &self.inner.nodes {
                let rect = Rect {
                    min: transform.graph_pos_to_screen_pos(node.rect.min),
                    max: transform.graph_pos_to_screen_pos(node.rect.max),
                };

                // Here we draw the node content (can be any widget)
                let mut child_ui = ui.child_ui(rect, Layout::top_down(egui::Align::Center));

                // Regarding show node value (which closure parameter to provide), call underlying closure
                Self::show_transformed(&mut child_ui, transform, &self.inner.config, |ui| {
                    match show_node {
                        ShowNode::Simple(show_node) => {
                            // Call show node closure made by the user (telling us what to draw in place of the node)
                            (show_node)(graph, NodeIndex::new(*index), ui, transform.zoom_factor);
                        }
                        ShowNode::WithState(show_node) => {
                            if let Some(state) = state {
                                // Call show node closure made by the user (telling us what to draw in place of the node)
                                (show_node)(
                                    graph,
                                    NodeIndex::new(*index),
                                    ui,
                                    transform.zoom_factor,
                                    state,
                                );
                            }
                        }
                        ShowNode::Interactive(show_node) => {
                            // Recovers the interactions
                            // NOTE: here it's important to have an id PER node
                            // NOTE: this make the node region loosing drag and double click capability (graph reposition/move)
                            let response = ui.interact(
                                rect,
                                self.id.with("nodes").with(index),
                                Sense::click(),
                            );

                            // Call show node closure made by the user (telling us what to draw in place of the node)
                            (show_node)(
                                graph,
                                NodeIndex::new(*index),
                                ui,
                                transform.zoom_factor,
                                response,
                            );
                        }
                        ShowNode::InteractiveWithState(show_node) => {
                            if let Some(state) = state {
                                // Recovers the interactions
                                // NOTE: here it's important to have an id PER node
                                // NOTE: this make the node region loosing drag and double click capability (graph reposition/move)
                                let response = ui.interact(
                                    rect,
                                    self.id.with("nodes").with(index),
                                    Sense::click(),
                                );

                                // Call show node closure made by the user (telling us what to draw in place of the node)
                                (show_node)(
                                    graph,
                                    NodeIndex::new(*index),
                                    ui,
                                    transform.zoom_factor,
                                    state,
                                    response,
                                );
                            }
                        }
                    }
                });
            }
        }
    }

    /// Show a transformed collection of widgets.
    ///
    /// This will hook the font system to force the right perspective (using the `TransformState`).
    fn show_transformed<F>(
        ui: &mut Ui,
        transform: &TransformState,
        config: &DrawableGraphConfig,
        mut show: F,
    ) where
        F: FnMut(&mut Ui),
    {
        // We compute the font size here, applying the zoom factor
        let font_size = config.text_font_size * transform.zoom_factor;

        // If the font_size is too small, we stop drawing the node content
        if font_size <= 1.0 {
            return;
        }

        // This is a hack (cf https://github.com/emilk/egui/issues/1811#issuecomment-1260473122)
        // Here we just force all text font to scale regarding our zoom factor
        let old_style = ui.style().clone();

        ui.style_mut()
            .text_styles
            .iter_mut()
            .for_each(|(_, current_font)| current_font.size = font_size);
        ui.style_mut().spacing.interact_size = Vec2::ZERO;
        ui.style_mut().spacing.item_spacing *= transform.zoom_factor;
        ui.style_mut().spacing.window_margin.bottom *= transform.zoom_factor;
        ui.style_mut().spacing.window_margin.top *= transform.zoom_factor;
        ui.style_mut().spacing.window_margin.right *= transform.zoom_factor;
        ui.style_mut().spacing.window_margin.left *= transform.zoom_factor;
        ui.style_mut().spacing.button_padding *= transform.zoom_factor;
        ui.style_mut().spacing.menu_margin.bottom *= transform.zoom_factor;
        ui.style_mut().spacing.menu_margin.top *= transform.zoom_factor;
        ui.style_mut().spacing.menu_margin.right *= transform.zoom_factor;
        ui.style_mut().spacing.menu_margin.left *= transform.zoom_factor;
        ui.style_mut().spacing.indent *= transform.zoom_factor;
        ui.style_mut().spacing.interact_size *= transform.zoom_factor;
        ui.style_mut().spacing.slider_width *= transform.zoom_factor;
        ui.style_mut().spacing.combo_width *= transform.zoom_factor;
        ui.style_mut().spacing.text_edit_width *= transform.zoom_factor;
        ui.style_mut().spacing.icon_width *= transform.zoom_factor;
        ui.style_mut().spacing.icon_width_inner *= transform.zoom_factor;
        ui.style_mut().spacing.icon_spacing *= transform.zoom_factor;
        ui.style_mut().spacing.tooltip_width *= transform.zoom_factor;
        ui.style_mut().spacing.combo_height *= transform.zoom_factor;
        ui.style_mut().spacing.scroll_bar_width *= transform.zoom_factor;
        ui.style_mut().spacing.scroll_handle_min_length *= transform.zoom_factor;
        ui.style_mut().spacing.scroll_bar_inner_margin *= transform.zoom_factor;
        ui.style_mut().spacing.scroll_bar_outer_margin *= transform.zoom_factor;

        // Here we call the closure
        (show)(ui);

        // And then we reset everything
        ui.set_style(old_style);
    }

    /// Automatically resize the position and zoom of the graph to fit it into the screen.
    ///
    /// This use """complex""" maths to compute the adequate zoom and position offset to use
    /// to fit the whole graph into the screen.
    fn auto_resize(&mut self, ui: &mut Ui, transform: &mut TransformState) {
        // Reset user interaction variable
        transform.user_modified = false;

        // If there is nothing to draw, size_min and/or size_max will be NaN (not valid)
        // In this case we do nothing
        if !self.inner.bounding_box.min.is_finite() || !self.inner.bounding_box.max.is_finite() {
            return;
        }

        let mut bounding_box = self.inner.bounding_box;
        bounding_box.max.x += self.node_margin.x;
        bounding_box.max.y += self.node_margin.y;
        bounding_box.min.x -= self.node_margin.x;
        bounding_box.min.y -= self.node_margin.y;

        // If maximum bound, the new min bound is max.x - maximum_bound_x
        if let Some(x_size) = self.maximum_bound_x {
            if bounding_box.max.x > x_size {
                bounding_box.min.x = bounding_box.max.x - x_size;
            }
        }

        // If maximum bound, the new min bound is max.y - maximum_bound_y
        if let Some(y_size) = self.maximum_bound_y {
            if bounding_box.max.y > y_size {
                bounding_box.min.y = bounding_box.max.y - y_size;
            }
        }

        // Available space on the current screen (size)
        let screen_size = Vec2::new(
            ui.available_size_before_wrap().x,
            ui.available_size_before_wrap().y,
        );

        // Zoom according to the screen and the size of the graph
        transform.zoom_factor = if ((bounding_box.max.x - bounding_box.min.x) / screen_size.x)
            > ((bounding_box.max.y - bounding_box.min.y) / screen_size.y)
        {
            // Resize X
            screen_size.x / (bounding_box.max.x - bounding_box.min.x)
        } else {
            // Resize Y
            screen_size.y / (bounding_box.max.y - bounding_box.min.y)
        };

        // Reset position offset
        transform.position_offset = Vec2::ZERO;

        // Middle of the graph in the screen, in function of the drag
        let middle = Pos2::new(
            (transform.graph_pos_to_screen_pos(bounding_box.min).x
                + transform.graph_pos_to_screen_pos(bounding_box.max).x)
                / 2.0,
            (transform.graph_pos_to_screen_pos(bounding_box.min).y
                + transform.graph_pos_to_screen_pos(bounding_box.max).y)
                / 2.0,
        );

        // Offset of the tab in the ui
        let offset_screen = Pos2::new(ui.next_widget_position().x, ui.next_widget_position().y);

        // Distance of the middle of the graph and the middle of the screen + offset
        let mut dist = Pos2::new(
            (ui.available_size_before_wrap().x / 2.0) - middle.x + offset_screen.x,
            (ui.available_size_before_wrap().y / 2.0) - middle.y + offset_screen.y,
        );

        // Movement of the graph divider by the zoom
        dist.x /= transform.zoom_factor;
        dist.y /= transform.zoom_factor;

        transform.position_offset = dist.to_vec2();
    }
}

impl<S, N, E, Ty: EdgeType, Ix: IndexType + Debug> Default for Graph<S, N, E, Ty, Ix> {
    fn default() -> Self {
        Self::new()
    }
}
