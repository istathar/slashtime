use gtk4 as gtk;
// glib and other dependencies are re-exported by the gtk crate
use gtk::prelude::*;

const APP_ID: &str = "org.gtk_rs.HelloWorld2";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = gtk::Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run()
}

fn build_ui(app: &gtk::Application) {
    // Create a button with label and margins
    let button = gtk::Button::builder()
        .label("Press me!")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();

    // Connect to "clicked" signal of `button`
    button.connect_clicked(|button| {
        // Set the label to "Hello World!" after the button has been clicked on
        button.set_label("Hello World!");
    });

    // Create a window
    let window = gtk::ApplicationWindow::builder()
        .application(app)
        .title("My GTK App")
        .child(&button)
        .build();

    // Present window
    window.present();
}
