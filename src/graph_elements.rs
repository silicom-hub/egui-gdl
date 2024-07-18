//! # Graph elements

use std::{collections::HashMap, f32::INFINITY};

use egui::{Pos2, Rect, Vec2};
use instant::Instant;
use layout::{
    adt::dag::NodeHandle,
    core::{base::Orientation, format::RenderBackend, geometry::Point, style::StyleAttr},
    std_shapes::shapes::{Arrow, Element, ShapeKind},
    topo::layout::VisualGraph,
};
use petgraph::{stable_graph::IndexType, EdgeType, Graph};
use std::fmt::Debug;

/// Configuration of a drawable graph.
#[derive(Clone)]
pub struct DrawableGraphConfig {
    /// Max size of a node (represents its shape).
    pub node_box_size: Vec2,
    /// Max size of an edge (represents its shape).
    pub edge_box_size: Vec2,
    /// Default text size.
    pub text_font_size: f32,
    /// Sampling for bezier curves.
    /// Number of sampling to apply to draw a bezier curve. Higher it is, smoother it will be rendered.
    pub bezier_curve_sampling: usize,
}

impl Default for DrawableGraphConfig {
    fn default() -> Self {
        Self {
            node_box_size: Vec2::new(150.0, 40.0),
            edge_box_size: Vec2::new(150.0, 40.0),
            text_font_size: 11.0,
            bezier_curve_sampling: 20, // NOTE: higher sampling render better graph lines, but impact performance
        }
    }
}

/// Helper struct that'll contain a petgraph graph.
/// TODO: maybe store a reference, or don't store it
/// TODO: maybe only apply graph addition here also? (to not update the whole graph all the time)
pub struct DrawableGraph<N, E, Ty: EdgeType, Ix: IndexType + Debug> {
    /// Rectangles to draw to represents nodes.
    pub nodes: HashMap<usize, DrawableNode>,
    /// Representing nodes links/graph edges.
    pub edges: HashMap<usize, DrawableEdge>,
    /// Collection of the lines created by the edges.
    pub lines: HashMap<usize, DrawableEdgesLines>,
    /// Inner petgraph graph.
    pub inner: Option<Graph<N, E, Ty, Ix>>,
    /// The bounding box of the drawable graph (graph coordinates, not screen coordinates).
    pub bounding_box: Rect,
    /// Configuration of the drawable graph.
    pub config: DrawableGraphConfig,
}

impl<N, E, Ty: EdgeType, Ix: IndexType + Debug> DrawableGraph<N, E, Ty, Ix> {
    /// Create a new drawable graph.
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            edges: HashMap::new(),
            lines: HashMap::new(),
            inner: None,
            bounding_box: Rect::NOTHING,
            config: Default::default(),
        }
    }

    /// Set a new inner graph.
    pub fn set_graph(&mut self, graph: Graph<N, E, Ty, Ix>) {
        self.inner = Some(graph);
    }

    /// Clear all drawable edges, nodes and links.
    ///
    /// NOTE: this keep the allocated memory, just clear elements and set len to 0.
    pub fn clear_drawable_graph(&mut self) {
        self.edges.clear();
        self.nodes.clear();
        self.lines.clear();
    }

    /// Update layout (to call if the inner graph changed).
    pub fn update(&mut self) {
        self.clear_drawable_graph();

        let Some(inner) = &mut self.inner else { return };

        // NOTE: if the graph is empty, there is nothing to do
        if inner.node_count() == 0 {
            return;
        }

        // Reset graph size
        self.bounding_box.max = Pos2::new(-INFINITY, -INFINITY);
        self.bounding_box.min = Pos2::new(INFINITY, INFINITY);

        // Reset nodes and edges
        // Since we clear (= we keep allocations) this just make sure we have enough space (otherwise do new allocation)
        self.nodes.reserve(inner.node_count());
        self.edges.reserve(inner.edge_count());

        // Convert the petgraph into a `layout::VisualGraph`
        let mut layouter = VisualGraph::new(Orientation::LeftToRight);
        let mut nodes = Vec::<NodeHandle>::with_capacity(inner.node_indices().count());

        // Convert nodes
        for i in inner.node_indices() {
            // We store the node index inside rounded
            // FIXME: this is a hack, either make a PR to layout or fork it?
            let mut style = StyleAttr::simple();
            style.rounded = i.index();

            let shape = ShapeKind::new_box("");
            let size = Point::new(
                self.config.node_box_size.x.into(),
                self.config.node_box_size.y.into(),
            );
            let element = Element::create(shape, style, Orientation::LeftToRight, size);
            let handle = layouter.add_node(element);
            nodes.push(handle);
        }

        // Convert edges
        for i in inner.edge_indices() {
            if let Some(endpoints) = inner.edge_endpoints(i) {
                let mut arrow = Arrow::simple(&i.index().to_string());

                // We store the node index inside font_size
                // FIXME: this is a hack, either make a PR to layout or fork it?
                arrow.look.font_size = i.index();

                // NOTE: this is a fix to make the graph left to right in all cases
                let (start, end) = if endpoints.0.index() > endpoints.1.index() {
                    (nodes[endpoints.1.index()], nodes[endpoints.0.index()])
                } else {
                    (nodes[endpoints.0.index()], nodes[endpoints.1.index()])
                };

                layouter.add_edge(arrow, start, end);
            }
        }

        self.config.bezier_curve_sampling =
            Self::optimize_bezier_curve_sampling(inner.edge_count());

        // Compute the graph layout
        // NOTE: "disable_opt" will improve performances a lot on huge graphs
        //       It doesn't impact that much the rendered graph
        let disable_opt = inner.node_count() > 50 && inner.edge_count() > 50;

        tracing::debug!("Layout starting...");
        let started = Instant::now();

        layouter.do_it(false, disable_opt, false, self);

        tracing::debug!("Layout finished in {:.2?}", started.elapsed());
    }

    /// Scale the bezier curve sampling value regarding the number of edges.
    /// NOTE: higher sampling render better graph lines, but impact performance
    fn optimize_bezier_curve_sampling(edges_count: usize) -> usize {
        if edges_count < 50 {
            1000
        } else if edges_count < 200 {
            100
        } else {
            20
        }
    }

    /// Check if position is in current bounding box, if not change the bounding box to include it.
    fn update_bound_pos(&mut self, position: &Pos2) {
        self.bounding_box.min = position.min(self.bounding_box.min);
        self.bounding_box.max = position.max(self.bounding_box.max);
    }

    /// Check if rect is in current bounding box, if not change the bounding box to include it.
    fn update_bound_rect(&mut self, rect: &Rect) {
        if rect.min.x < self.bounding_box.min.x {
            self.bounding_box.min.x = rect.min.x;
        };
        if rect.min.y < self.bounding_box.min.y {
            self.bounding_box.min.y = rect.min.y;
        };
        if rect.max.x > self.bounding_box.max.x {
            self.bounding_box.max.x = rect.max.x;
        };
        if rect.max.y > self.bounding_box.max.y {
            self.bounding_box.max.y = rect.max.y;
        };
    }
}

// Used to convert a layout::VisualGraph into a drawable graph (layout will do the placement).
// FIXME: everything here is very hacky...
impl<N, E, Ty: EdgeType, Ix: IndexType + Debug> RenderBackend for DrawableGraph<N, E, Ty, Ix> {
    fn draw_rect(
        &mut self,
        xy: Point,
        size: Point,
        look: &StyleAttr,
        _clip: Option<layout::core::format::ClipHandle>,
    ) {
        let rect = Rect::from_min_max(
            Pos2 {
                x: xy.x as f32,
                y: xy.y as f32,
            },
            Pos2 {
                x: xy.add(size).x as f32,
                y: xy.add(size).y as f32,
            },
        );

        // Compute the bounding box of the graph
        self.update_bound_rect(&rect);

        // We stored the node index inside rounded
        // FIXME: this is a hack, either make a PR to layout or fork it?
        self.nodes.insert(look.rounded, DrawableNode::new(rect));
    }

    fn draw_line(&mut self, _: Point, _: Point, _look: &StyleAttr) {}

    fn draw_circle(&mut self, _: Point, _: Point, _look: &StyleAttr) {}

    fn draw_text(&mut self, xy: Point, text: &str, _look: &StyleAttr) {
        // If there is a text and that is parsable to and index, then it's an edge text location
        // FIXME: this is a hack, either make a PR to layout or fork it?
        if let Ok(index) = text.parse::<usize>() {
            self.edges.insert(
                index,
                DrawableEdge::new(Pos2::new(xy.x as f32, xy.y as f32)),
            );
        }
    }

    fn draw_arrow(
        &mut self,
        path: &[(Point, Point)],
        _: bool,
        _: (bool, bool),
        style: &StyleAttr,
        _: &str,
    ) {
        let links = path
            .to_vec()
            .iter()
            .map(|(first, second)| {
                let first = Pos2::new(first.x as f32, first.y as f32);
                let second = Pos2::new(second.x as f32, second.y as f32);

                // Compute the bounding box of the graph
                self.update_bound_pos(&first);
                self.update_bound_pos(&second);

                (first, second)
            })
            .collect();

        // We retrieve the id of the link from the font_size
        // FIXME: this is a hack, either make a PR to layout or fork it?
        self.lines.insert(
            style.font_size,
            DrawableEdgesLines::new(links, self.config.bezier_curve_sampling),
        );
    }

    // Here we don't use layout clip functionality/styling, so let's just ignore it (= do nothing when drawing)
    // We don't miss anything here since the construction of `layout::VisualGraph` doesn't create any clip rect
    fn create_clip(&mut self, _: Point, _: Point, _: usize) -> layout::core::format::ClipHandle {
        0
    }
}

impl<N, E, Ty: EdgeType, Ix: IndexType + Debug> Default for DrawableGraph<N, E, Ty, Ix> {
    fn default() -> Self {
        Self::new()
    }
}

/// Node of a visual graph (drawable).
#[derive(Clone)]
pub struct DrawableNode {
    /// Struct rect for the position of the rectangle.
    pub rect: Rect,
}

impl DrawableNode {
    /// Helper to create a new node.
    pub fn new(rect: Rect) -> Self {
        Self { rect }
    }
}

/// Edge of a visual graph (drawable).
#[derive(Clone)]
pub struct DrawableEdge {
    /// Content position of the edge (e.g. where to place text on a arrow).
    pub content_position: Pos2,
}

impl DrawableEdge {
    /// Helper to create a new arrow.
    pub fn new(content_position: Pos2) -> Self {
        Self { content_position }
    }
}

/// Store a collection of drawable edges links.
/// Can contain all the lines/points of all of the edges of the graph.
pub struct DrawableEdgesLines {
    /// Set of self.points that define the arrow (bezier curves).
    points: Vec<(Pos2, Pos2)>,
    /// List of line from a sampled bezier curve.
    sampled_lines: Vec<Pos2>,
    /// Sampling for the bezier curves.
    bezier_curves_sampling: usize,
}

impl DrawableEdgesLines {
    /// Helper to create a new arrow.
    pub fn new(points: Vec<(Pos2, Pos2)>, bezier_curves_sampling: usize) -> Self {
        let mut this = Self {
            points,
            sampled_lines: Vec::new(),
            bezier_curves_sampling,
        };
        this.compute_bezier_curve();
        this
    }

    /// Update the drawable edge.
    #[allow(dead_code)] // This function will be used in a cache system
    pub fn update(&mut self, points: Vec<(Pos2, Pos2)>) {
        self.points = points;
        self.compute_bezier_curve();
    }

    /// Get the sampled lines from this drawable edge.
    pub fn sampled_lines(&self) -> &[Pos2] {
        &self.sampled_lines
    }

    /// Convert a set of lines to smooth bezier curves.
    ///
    /// <https://fr.wikipedia.org/wiki/Courbe_de_B%C3%A9zier>
    fn compute_bezier_curve(&mut self) {
        if self.points.is_empty() {
            return;
        }

        let len = self.points.len() - 2;

        self.sampled_lines.clear();
        self.sampled_lines
            .reserve(len * self.bezier_curves_sampling);

        for i in 0..=len {
            let p0 = if i == 0 {
                self.points[i].0
            } else {
                self.points[i].1
            };
            let p1 = if i == 0 {
                self.points[i].1
            } else {
                Pos2::new(
                    self.points[i].0.x + ((self.points[i].1.x - self.points[i].0.x) * 2.0),
                    self.points[i].0.y + ((self.points[i].1.y - self.points[i].0.y) * 2.0),
                )
            };
            let p2 = self.points[i + 1].0;
            let p3 = self.points[i + 1].1;

            for step in 0..=self.bezier_curves_sampling {
                let t = step as f32 / self.bezier_curves_sampling as f32;
                let x = (1.0 - t).powi(3) * p0.x
                    + 3.0 * t * (1.0 - t).powi(2) * p1.x
                    + 3.0 * t.powi(2) * (1.0 - t) * p2.x
                    + t.powi(3) * p3.x;
                let y = (1.0 - t).powi(3) * p0.y
                    + 3.0 * t * (1.0 - t).powi(2) * p1.y
                    + 3.0 * t.powi(2) * (1.0 - t) * p2.y
                    + t.powi(3) * p3.y;

                self.sampled_lines.push(Pos2::new(x, y));
            }
        }
    }
}
