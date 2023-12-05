use glib::clone;
use gtk::{
    prelude::*, Application, ApplicationWindow, EventControllerMotion, GestureClick, IconTheme,
    Label, ListView, PolicyType, ScrolledWindow, SignalListItemFactory, SingleSelection,
    StringList, StringObject,
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

    // Intermediate between the underlying model and the view is a
    // SelectionModel. We set that up so that by default nothing is selected,
    // but when the mouse goes over a row that row becomes selected, again
    // unselecting on exit.

    let selection = SingleSelection::builder()
        .model(&model)
        .autoselect(false)
        .can_unselect(true)
        .build();

    let pos = selection.selected();
    selection.unselect_item(pos);

    // Capture motion events so we can react to the mouse pointer leaving the window.
    let motion = EventControllerMotion::new();

    // Connect the leave event
    motion.connect_leave(clone!(@weak selection => move |_| {
        // for some reason, unselect_all() doesn't work here, returning false.
        // So we get the currently selected row, explicitly call
        // unselect_item() on it, and that works.
        let pos = selection.selected();
        selection.unselect_item(pos);
    }));

    // Connect to what used to be the 'button-press-event' signal for right-clicks
    let gesture = GestureClick::builder()
        .button(gdk::BUTTON_SECONDARY) // right click
        .build();

    gesture.connect_pressed(clone!(@weak selection => move |_, _, x, y| {
        let row = selection.selected();
        println!("Right click at {},{} row {}", x, y, row);
    }));

    let view = ListView::builder()
        .model(&selection)
        .factory(&factory)
        .single_click_activate(true)
        .build();

    // unclear whether we need a ScrolledWindow or not. It may be unnecessary.
    let scrolled = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        .vscrollbar_policy(PolicyType::Never)
        .min_content_width(360)
        .child(&view)
        .build();

    // Sort out the icon for the application.
    let display = gdk::Display::default().expect("Unable to get default display");
    let theme = IconTheme::for_display(&display);
    theme.add_search_path("./share/icons/hicolor");
    theme.add_resource_path("slashtime.png");

    let window = ApplicationWindow::builder()
        .application(app)
        .decorated(false)
        .title("Slashtime")
        .icon_name("slashtime")
        .default_width(600)
        .default_height(300)
        .child(&scrolled)
        .build();

    // Connect to what used to be the 'row-activated' signal
    view.connect_activate(move |_, row| {
        println!("Left click on row {}", row);
    });

    view.add_controller(gesture);

    view.add_controller(motion);

    window.present();
}
