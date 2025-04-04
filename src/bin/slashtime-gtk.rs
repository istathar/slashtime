use gio;
use glib::clone;
use glib::ControlFlow;
use gtk::{
    prelude::*, Application, ApplicationWindow, EventControllerMotion, GestureClick, IconTheme,
    Label, ListView, PolicyType, ScrolledWindow, SignalListItemFactory, SingleSelection,
    StringObject,
};
use slashtime::find_home;
use slashtime::format_line;
use slashtime::Locality;
use std::cell::RefCell;
use std::rc::Rc;
use tz::DateTime;
use tz::TimeZoneRef;

use slashtime::object::LocalityObject;

const APP_ID: &str = "org.aesiniath.Slashtime";

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder()
        .application_id(APP_ID)
        .build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_ui);

    // Run the application
    app.run()
}

fn build_ui(app: &Application) {
    let locations = slashtime::loading::load_tzlist().unwrap();

    let home = find_home(&locations).unwrap();
    let now = tz::UtcDateTime::now().unwrap();

    let home = Rc::new(RefCell::new(home.clone()));
    let when = Rc::new(RefCell::new(now));

    let model: gio::ListStore = gio::ListStore::new::<LocalityObject>();

    let factory = SignalListItemFactory::new();

    // Setup signal: Create and initialize widgets
    factory.connect_setup(move |_, object| {
        let label = Label::new(None);
        let item = object
            .downcast_ref::<gtk::ListItem>()
            .expect("The object should be a ListItem");
        item.set_child(Some(&label));
    });

    // Probably unnecessary but included for correctness
    factory.connect_teardown(move |_, object| {
        let item = object
            .downcast_ref::<gtk::ListItem>()
            .expect("The object should be a ListItem");
        item.set_child(None::<&gtk::Label>);
    });

    // Bind signal: Bind data to widgets
    factory.connect_bind(clone!(@weak home => move |_, object| {
        let item = object
            .downcast_ref::<gtk::ListItem>()
            .expect("The object should be a ListItem");

        let actual = item
            .item()
            .expect("The ListItem's item should be present")
            .downcast::<LocalityObject>()
            .expect("The ListItem's item should be a LocalityObject");

        let label = item
            .child()
            .expect("The ListItem's child should be present")
            .downcast::<Label>()
            .expect("The ListItem's child should be a Label");

        // now format the text to go into the rows

        let location = actual.get();

        let from = home.borrow();

        let there = when
            .borrow()
            .project(
                location
                    .zone
                    .as_ref(),
            )
            .unwrap();

        let string = format_line(&location, &from, &there);

        label.set_label(&string);
    }));

    // Probably unnecessary but included for correctness
    factory.connect_unbind(move |_, object| {
        let item = object
            .downcast_ref::<gtk::ListItem>()
            .expect("The object should be a ListItem");

        let label = item
            .child()
            .expect("The ListItem's child should be present")
            .downcast::<Label>()
            .expect("The ListItem's child should be a Label");

        label.set_label("");
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
        .single_click_activate(false)
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
    view.connect_activate(
        clone!(@strong locations, @weak model, @weak home => move |_, row| {
            // update the shared mutable reference "home" to point to the new location
            *home.borrow_mut() = locations[row as usize].clone();

            // repopulate model to update to new home accordingly
            let utc = TimeZoneRef::utc();
            let now = tz::DateTime::now(utc).unwrap();

            populate_model(&model, &locations);
        }),
    );
    view.add_controller(gesture);

    view.add_controller(motion);

    // Initial data insertion
    populate_model(&model, &locations);

    // Setup a timer to refresh the data every second
    glib::timeout_add_local(
        std::time::Duration::from_millis(1000),
        move || {
            let utc = TimeZoneRef::utc();
            let now = tz::DateTime::now(utc).unwrap();

            if now.second() == 0 {
                populate_model(&model, &locations);
            }
            ControlFlow::Continue
        },
    );

    window.present();
}

fn populate_model(model: &gio::ListStore, locations: &[Locality]) {
    // remove existing entries
    model.remove_all();

    // (re)load new entries
    for location in locations {
        let object = slashtime::object::LocalityObject::new(&location);

        model.append(&object);
    }
}
