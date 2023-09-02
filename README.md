# Plugboard - a plugin utility

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

* Create "extendable" classes with a special metaclass (optional)
* Create "plugin" classes with a special metaclass
* When a plugin is loaded, call `reinitialize-instance` on the extendable class
to add the plugin class as a direct superclass
* Voila! New and existing objects of the extendable class now get the plugin behavior

Why use metaclasses? It isn't needed, per se, but it allows us to have some
bonus behavior. We can **reset** extendable classes by removing all plugin classes
from the direct superclasses. Using the `EXTENDABLE-CLASS` metaclass and associated
top-level object class `EXTENDABLE-OBJECT` are strictly optional; however, these
classes are required to make `ON-ENABLED` and `ON-DISABLED` callbacks on instances
work. 

Actually extending behavior is then done through generic functions. Since we add our
plugin classes as superclasses, the base extendable is the most-specific, followed
by the plugin classes. We'll probably have to be intentional about the order we
add the plugin superclasses.

## API

Plugin definition and usage:

* `plugin-class`: a metaclass for plugin classes, with predicate `(plugin-class-p class)`.
* `(defplugin name direct-superclasses direct-slots options)`: defines a plugin class.
Like `defclass`, but sets the metaclass and handles automatically enabling the plugin.
Two additional class options are permitted:
  * `:extends <extendables>`, which takes a class name or list of class names to extend.
  * `:enable <T/NIL>` (default `T`), which specifies whether or not to enable the plugin
  by default on all classes listed in the `:extends` option.
* `(enable plugin &optional extendable)`: enables a plugin by redefining the extendable
class(es).

Extendable object definition and usage:

* `extendable-class`: a metaclass for extendable classes, with predicate `(extendable-class-p class)`.
* `extendable-object`: a mixin class for extendable objects, with predicate `(extendablep object)`.
* `(defextendable name direct-superclasses direct-slots options)`: defines an extendable class
with the proper metaclass and superclass.
* `(disable plugin &optional extendable)`: disables a plugin by redefining the extendable
class(es).
* `(enabled-plugins extendable)`: returns a list of all plugin class instances currently
enabled on `extendable`.
* `(disable-all-plugins extendable)`: disables all plugins enabled on `extendable`.
* `(on-enabled extendable-instance plugin-class)` [generic function]: called on extendable instances when
plugin class `PLUGIN-CLASS` is enabled. Also called for each enabled plugin when a new instance is created.
* `(on-disabled extendable-instance plugin-class property-list)` [generic function]: called on
extendable instances when plugin class `PLUGIN-CLASS` is disabled. `PROPERTY-LIST` contains a plist of
at least the slots and values from `PLUGIN-CLASS` that were removed from `EXTENDABLE-INSTANCE`.

> [!NOTE]
> The `ON-ENABLED` and `ON-DISABLED` callbacks use the `UPDATE-INSTANCE-FOR-REDEFINED-CLASS`
> machinery, so the exact time these callbacks happen is implementation-defined, but no later
> than the next time a slot is accessed in the instance (see CLHS 4.3.6).

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
`systems.duck.plugboard`.

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
> class. Likewise, if a plugin is disabled, the slots are removed and wiped. To hook into these
> updates, define the extendable class with `DEFEXTENDABLE` and define methods on `ON-ENABLED`
> and `ON-DISABLED`, which get called on instances when the active plugin set changes.

## ASDF Extension

> [!WARNING]
> This ASDF is experimental and subject to change! Requires ASDF 3.3.5 or later for full functionality.

Instead of hard-coding plugins to load in your system definition, the
`systems.duck.plugboard/asdf` extension adds a `:PLUGBOARD` component which reads plugin 
system names out of a text file. This file is easier to update during a build process
when the specific list of plugins is not specifically known ahead of time. Use:

```lisp
(asdf:defsystem "systems.duck.your-system"
  ; ...
  :defsystem-depends-on ("systems.duck.plugboard/asdf")
  ; ...
  :components ((:plugboard "plugins.txt")
               ; ...
               ))
```

where `plugins.txt` contains a single plugin system name on each line, e.g.:

```
systems.duck.example.plugin1
systems.duck.example.plugin2
```
