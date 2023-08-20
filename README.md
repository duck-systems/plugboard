# Plug - a plugin utility

I needed a plugin system, but the existing options weren't meeting my needs.

## Alternatives

1. **Hooks**: Defining plugins through hooks requires a lot of setup and
maintenance: tracking the active hooks, iterating and calling, etc. Common Lisp
already has a great extensibility mechanism built in (CLOS), so let's use it!
2. **piddling-plugins**: A good option using CLOS, and a great source of
inspiration. However, it relies on changing the class of particular object
instances (and specifically a global `*application*` object. I am more interested
in changing the actual _class_ objects, so that I can have arbitrary pluggable
classes and instances. That is, the plugin class must know at load time the
specific _thing_ it needs to extend, which classes are better suited for.

## Philosophy

* Create "extendable" classes ~with a special metaclass~ (might be added later)
* Create "plugin" classes with a special metaclass
* When a plugin is loaded, call `reinitialize-instance` on the extendable class
to add the plugin class as a direct superclass
* Voila! New and existing objects of the extendable class now get the plugin behavior

Why use metaclasses? It isn't needed, per se, but it allows us to have some
bonus behavior. We can **reset** extendable classes by removing all plugin classes
from the direct superclasses. I can't think of a specific reason to have an extendable
metaclass, though, other than documentation-related things? Or options like don't
automatically enable plugins on load? Unclear.

Actually extending behavior is then done through generic functions. Since we add our
plugin classes as superclasses, the base extendable is the most-specific, followed
by the plugin classes. We'll probably have to be intentional about the order we
add the plugin superclasses.

## API

* `plugin-class`: a metaclass for plugin classes, with predicate `(plugin-class-p class)`.
* `(defplugin name direct-superclasses direct-slots options)`: defines a plugin class.
Like `defclass`, but sets the metaclass and handles automatically enabling the plugin.
Two additional class options are permitted:
  * `:extends <extendables>`, which takes a class name or list of class names to extend.
  * `:enable <T/NIL>` (default `T`), which specifies whether or not to enable the plugin
  by default on all classes listed in the `:extends` option.
* `(enable plugin &optional extendable)`: enables a plugin by redefining the extendable
class(es).
* `(disable plugin &optional extendable)`: disables a plugin by redefining the extendable
class(es).
* `(enabled-plugins extendable)`: returns a list of all plugin class instances currently
enabled on `extendable`.
* `(disable-all-plugins extendable)`: disables all plugins enabled on `extendable`.
All `plugin` and `extendable` arguments can be either a class name (symbol) or instance.
Additionally, `enable` and `disable` accept `NIL` as shorthand for all classes specified in
the plugin's `:extends` class option, as well as a list of class names/instances to process
in bulk. In `enabled-plugins` and `disable-all-plugins`, instances of the extendable class
are also accepted.

## Notes

This library depends on `closer-mop`, and therefore is portable across all implementations
supported by `closer-mop`. It is expected this library is used with package local nicknames
or similar, not just `use`d. That way, the API functions are, e.g., `plug:enable` or similar.

## Examples

In the following examples, it is assumed that `plug` is defined as a local nickname for
`systems.duck.plug`.

Plugin for providing new command line options:
```lisp
;;; Define a class we will extend
(defclass command-line ()
  ((built-in-options :reader built-in-options
                    :initform (list "Option 1" "Option 2" "Option 3")
                    :documentation "List of built-in options for this command"))
  (:documentation "A class implementing a command line processor"))

;;; Define a generic function. We want plugins to be able to add to the list of options,
;;;  so we use a method combination of APPEND to accumulate results from all plugins
(defgeneric options (cmd)
  (:documentation "Returns a list of options for CMD")
  (:method-combination append))

;;; Define the primary method on the extendable class
(defmethod options append ((cmd command-line))
  (built-in-options cmd))

;;; Define a plugin
(plug:defplugin bonus-options ()
  () ; No slots
  (:documentation "A plugin that provides a couple bonus options")
  (:extends command-line))

;;; Define an extension with a method
(defmethod options append ((plugin bonus-options))
  (list "Bonus 1" "Bonus 2"))

;;; Define another plugin, which provides randomized options
(plug:defplugin random-options ()
  ((words :reader words
          :initform (list "Jean" "Francine" "Bill" "Frank")
          :documentation "Words that will be randomly chosen"))
  (:extends command-line)
  (:documentation "A plugin that provides randomized options"))

;;; Define a method that randomly chooses a word
(defmethod options append ((plugin random-options))
  (list (elt (words plugin) (random (length (words plugin))))))

;;; Check and see
(options (make-instance 'command-line))
;; => ("Option 1" "Option 2" "Option 3" "Frank" "Bonus 1" "Bonus 2")
```

> [!NOTE]
> As plugins get added as superclasses, any slots defined in plugins get added to the extending
> class. Likewise, if a plugin is disabled, the slots are removed and wiped. You can easily
> hook into the enabling process by defining methods on `SHARED-INITIALIZE` or
> `UPDATE-INSTANCE-FOR-REDEFINED-CLASS`, but hooking into the disabling process is much harder.
> Since the extended class itself is _redefined_, the existing class with the plugin no longer
> exists by the time instances are being updated, so you cannot specialize on the plugin class.
> That might require a second plugin to watch for plugin changes...
