use std::collections::LinkedList;
use core::ops::CoerceUnsized;
use core::marker::Unsize;

/// Instance of a garbage collected heap

pub struct GcHeap {
  objects: LinkedList<Box<GcAlloc<dyn GcObj>>>
}

/// Pointer to a garbage collected object

pub struct GcPtr<T: ?Sized + GcObj>(*mut GcAlloc<T>);

/// Garbage collected allocation

#[repr(C)]
struct GcAlloc<T: ?Sized + GcObj> {
  mark: bool,
  val: T
}

/// Must be implemented by all GC'd objects

pub trait GcObj {
  fn visit_children(&self, _: fn(GcPtr<dyn GcObj>)) {}
}

impl GcHeap {
  pub fn new() -> Self {
    Self { objects: LinkedList::new() }
  }

  pub fn alloc<T: 'static + GcObj>(&mut self, val: T) -> GcPtr<T> {
    unsafe {
      // Create allocation
      let mem = Box::into_raw(Box::new(GcAlloc {
        mark: false,
        val: val
      }));
      // Store internal reference
      self.objects.push_back(Box::from_raw(mem));
      // Return GC'd pointer
      GcPtr(mem)
    }
  }

  pub fn collect(&mut self, root_obj: GcPtr<dyn GcObj>) {
    self.mark(root_obj);
    self.sweep();
  }

  // I. Mark reachable objects
  fn mark(&mut self, root_obj: GcPtr<dyn GcObj>) {
    fn mark(ptr: GcPtr<dyn GcObj>) {
      unsafe {
        // If the object wasn't marked before
        if (*ptr.0).mark == false {
          // Mark it
          (*ptr.0).mark = true;
          // And mark any children it might have
          (*ptr.0).val.visit_children(mark);
        }
      }
    }
    mark(root_obj);
  }

  // II. Sweep unmarked objects
  fn sweep(&mut self) {
    let mut cursor = self.objects.cursor_front_mut();
    while let Some(object) = cursor.current() {
      if object.mark == false {
        // If the object was not marked, free it
        cursor.remove_current();
      } else {
        // Otherwise remove the mark
        object.mark = false;
      }
      // Forward cursor
      cursor.move_next();
    }
  }
}

impl<T: ?Sized + Unsize<U> + GcObj, U: ?Sized + GcObj>
  CoerceUnsized<GcPtr<U>> for GcPtr<T> {}

impl<T: ?Sized + GcObj> Clone for GcPtr<T> {
  fn clone(&self) -> Self {
    Self(self.0)
  }
}

impl<T: ?Sized + GcObj> Copy for GcPtr<T> {}

impl<T: ?Sized + GcObj> core::borrow::Borrow<T> for GcPtr<T> {
  fn borrow(&self) -> &T {
    unsafe { &(*self.0).val }
  }
}

impl<T: ?Sized + GcObj> core::borrow::BorrowMut<T> for GcPtr<T> {
  fn borrow_mut(&mut self) -> &mut T {
    unsafe { &mut (*self.0).val }
  }
}

impl<T: ?Sized + GcObj> core::ops::Deref for GcPtr<T> {
  type Target = T;

  fn deref(&self) -> &T {
    unsafe { &(*self.0).val }
  }
}

impl<T: ?Sized + GcObj> core::ops::DerefMut for GcPtr<T> {
  fn deref_mut(&mut self) -> &mut T {
    unsafe { &mut (*self.0).val }
  }
}
