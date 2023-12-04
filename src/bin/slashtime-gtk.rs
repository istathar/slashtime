use gtk::{glib, prelude::*};
use gtk::{
    Application, ApplicationWindow, Label, ListView, NoSelection, PolicyType, ScrolledWindow,
    SignalListItemFactory, StringList, StringObject,
};
use slashtime::format_line;

const APP_ID: &str = "org.aesiniath.Slashtime";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run()
}

fn build_ui(app: &Application) {
    let now = tz::UtcDateTime::now().unwrap();

    let locations = slashtime::loading::load_tzlist().unwrap();

    let mut strings: Vec<String> = Vec::with_capacity(locations.len());

    for location in locations {
        let there = now.project(location.zone.as_ref()).unwrap();
        let string = format_line(&location, &there);
        strings.push(string);
    }

    // convert Strings to &str and then load StringList from there. Ugh.
    let slices: Vec<&str> = strings.iter().map(|s| s.as_str()).collect();
    let model: StringList = StringList::new(slices.as_slice());

    let factory = SignalListItemFactory::new();

    // Setup signal: Create and initialize widgets
    factory.connect_setup(move |_, list_item| {
        let label = Label::new(None);
        list_item.set_child(Some(&label));
    });

    // Bind signal: Bind data to widgets
    factory.connect_bind(move |_, list_item| {
        if let Some(item) = list_item.item() {
            let object = item
                .downcast_ref::<StringObject>()
                .expect("The item should be a StringObject");
            if let Some(label) = list_item
                .child()
                .and_then(|child| child.downcast::<Label>().ok())
            {
                label.set_label(&object.string());
            }
        }
    });

    let selection_model = NoSelection::new(Some(model));
    let list_view = ListView::new(Some(selection_model), Some(factory));

    let scrolled_window = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .min_content_width(360)
        .child(&list_view)
        .build();

    let window = ApplicationWindow::builder()
        .application(app)
        .title("My GTK App")
        .default_width(600)
        .default_height(300)
        .child(&scrolled_window)
        .build();

    window.present();
}
