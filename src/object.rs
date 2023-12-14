use glib::Object;
use gtk::prelude::*;
use gtk::subclass::prelude::*;
use super::Locality;

mod imp {
    use glib::subclass::prelude::*;

    // Object holding the state
    #[derive(Default)]
    pub struct LocalityObject;

    // The central trait for subclassing a GObject
    #[glib::object_subclass]
    impl ObjectSubclass for LocalityObject {
        const NAME: &'static str = "SlashtimeLocalityObject";
        type Type = super::LocalityObject;
        type ParentType = glib::Object;
    }

    // Trait shared by all GObjects
    impl ObjectImpl for LocalityObject {}
}

glib::wrapper! {
    pub struct LocalityObject(ObjectSubclass<imp::LocalityObject>);
}

impl LocalityObject {
    pub fn new() -> Self {
        glib::Object::new()
    }
}

impl Default for LocalityObject {
    fn default() -> Self {
        glib::Object::new()
    }
}
