use std::borrow::Borrow;

use super::Locality;
use glib;
use gtk::subclass::prelude::*;

mod imp {
    use glib;
    use glib::subclass::prelude::*;
    use std::cell::RefCell;

    // Object holding the state
    #[derive(Default)]
    pub struct LocalityObject {
        pub location: RefCell<super::Locality>,
    }

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
    pub fn new(location: &Locality) -> Self {
        let object: LocalityObject = glib::Object::new();
        let imp = imp::LocalityObject::from_obj(&object);
        imp.location
            .replace(location.clone());
        object
    }

    pub fn get(&self) -> super::Locality {
        let imp = self.imp();
        imp.location
            .borrow()
            .clone()
    }
}
