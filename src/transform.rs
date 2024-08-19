//! # Graph transform

use egui::{pos2, vec2, Id, Pos2, Rect, Style, Ui, Vec2};
use std::sync::Arc;

/// This should be cheap to clone.
/// One each frame draw this will clone the struct, update it and restore it.
#[derive(Clone)]
pub struct TransformState {
    /// Id of the current zoom object used to store and load data from the global app state.
    pub id: Id,
    /// Offset of the position (`screen_x = real_x + position_offset.x`).
    pub position_offset: Vec2,
    /// Ratio of `screen.?? / max_size.??` used to transform everything that is drawn with an offset of `real_position * zoom`.
    pub zoom_factor: f32,
    /// This is the rect used to clip the view only on a part of the graph. Anything that isn't in this rect will not be draw.
    pub draw_rect: Rect,
    /// Default style to use.
    pub default_style: Arc<Style>,
    /// Style to use when zooming.
    pub zoomed_style: Arc<Style>,
    /// Do the user performed any action?
    pub user_modified: bool,
}

/// Trait for zoom and drag.
impl TransformState {
    /// Charge the transform.
    pub fn load_from_ui_memory(ui: &mut Ui, id: Id) -> Option<TransformState> {
        ui.data_mut(|store| store.get_temp(id))
    }

    /// Save the transform.
    pub fn store_in_ui_memory(self, ui: &mut Ui) {
        ui.data_mut(|i| i.insert_temp(self.id, self));
    }

    /// Default transform with nothing.
    pub fn new(id: Id, ui: &mut Ui) -> Self {
        Self {
            id,
            position_offset: Vec2::ZERO,
            zoom_factor: 1.,
            draw_rect: Rect::NOTHING,
            default_style: ui.style().clone(),
            zoomed_style: ui.style().clone(),
            user_modified: false,
        }
    }

    /// Convert graph position into screen position.
    pub fn graph_pos_to_screen_pos(&self, p: Pos2) -> Pos2 {
        let Pos2 { x, y } = p;
        let x = x + self.draw_rect.left();
        let y = y + self.draw_rect.top();
        let x = (x + self.position_offset.x) * self.zoom_factor;
        let y = (y + self.position_offset.y) * self.zoom_factor;
        pos2(x, y)
    }

    /// Convert screen position into graph position.
    pub fn screen_pos_to_graph_pos(&self, p: Pos2) -> Pos2 {
        let Pos2 { x, y } = p;
        let x = x / self.zoom_factor - self.position_offset.x;
        let y = y / self.zoom_factor - self.position_offset.y;
        let x = x - self.draw_rect.left();
        let y = y - self.draw_rect.top();
        pos2(x, y)
    }

    /// Update the screen with the `Vec2` of the drag.
    pub fn drag(&mut self, delta: Vec2) {
        let delta = self.screen_vec_to_graph_vec(delta);
        self.update(self.position_offset + delta, self.zoom_factor);
    }

    /// Update the screen with the zoom.
    pub fn zoom(&mut self, screen_pos: Pos2, zoom: f32) {
        let new_zoom = (self.zoom_factor * zoom).clamp(0.01, 100.);

        // keep the screen_pos remain at the same location
        // solved for the equations: a2s_pos_zoom(s2a_pre_zoom(screen_pos)) = screen_pos

        let test = self.screen_pos_to_graph_pos(screen_pos);

        let Pos2 { x, y } = screen_pos;
        let new_position_offset = vec2(
            x / new_zoom - x / self.zoom_factor + self.position_offset.x,
            y / new_zoom - y / self.zoom_factor + self.position_offset.y,
        );

        let err = self.graph_pos_to_screen_pos(test) - screen_pos;
        assert!(err.x < 1.);
        assert!(err.y < 1.);
        self.update(new_position_offset, new_zoom);
    }

    /// Update the screen with position offset and zoom factor.
    pub fn update(&mut self, position_offset: Vec2, zoom_factor: f32) {
        self.position_offset = position_offset;
        self.zoom_factor = zoom_factor;
    }

    /// Show contents using this transformation.
    pub fn show<R, F>(&self, ui: &mut Ui, add_content: F) -> R
    where
        F: FnOnce(&mut Ui) -> R,
    {
        let original_cliprect = ui.clip_rect();
        ui.set_clip_rect(self.draw_rect);
        ui.ctx().set_style(self.zoomed_style.clone());
        let response = add_content(ui);
        ui.ctx().set_style(self.default_style.clone());
        ui.set_clip_rect(original_cliprect);

        response
    }

    /// Convert a vector (like a drag vector) of the screen into the graph area.
    /// For example, for a drag vector from the screen it will divide everything
    /// with the zoom factor to convert the vector into the graph space.
    pub fn screen_vec_to_graph_vec(&self, v: Vec2) -> Vec2 {
        vec2(v.x / self.zoom_factor, v.y / self.zoom_factor)
    }
}
