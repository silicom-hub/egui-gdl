use eframe::egui;
use egui::*;
use egui_gdl::*;

pub type GraphType = petgraph::Graph<String, ()>;

struct MyApp {
    pub graph: Graph<(), String, ()>,
}

impl Default for MyApp {
    fn default() -> Self {
        let mut initial_graph = GraphType::new();
        let pg = initial_graph.add_node("petgraph".to_string());
        let fb = initial_graph.add_node("fixedbitset".to_string());
        let qc = initial_graph.add_node("quickcheck".to_string());
        let rand = initial_graph.add_node("rand".to_string());
        let libc = initial_graph.add_node("libc".to_string());
        initial_graph.extend_with_edges([(pg, fb), (pg, qc), (qc, rand), (rand, libc), (qc, libc)]);

        let mut graph = Graph::new()
            .set_node_size(vec2(75.0, 75.0))
            .set_node_margin(vec2(0.0, 20.0))
            .show_node(
                |graph: &mut petgraph::Graph<String, ()>, index, ui: &mut Ui, zoom_factor| {
                    let name = graph.node_weight(index).unwrap();

                    ui.painter().rect_filled(
                        ui.available_rect_before_wrap(),
                        100. * zoom_factor,
                        Color32::WHITE,
                    );

                    ui.add_space(75.0 * zoom_factor);

                    ui.centered_and_justified(|ui| {
                        ui.label(name);
                    });
                },
            );
        graph.update_graph(initial_graph);

        Self { graph }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Hello world graphs!!!");
            self.graph.show(ui, None);
        });
    }
}

fn main() {
    let options = eframe::NativeOptions {
        ..Default::default()
    };
    eframe::run_native(
        "Hello world",
        options,
        Box::new(|_| {
            // This gives us image support:
            Box::<MyApp>::default()
        }),
    )
    .unwrap();
}
