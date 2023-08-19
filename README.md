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

* Create "extendable" (name TBD) classes with a special metaclass
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

* `defplugin`: defines a plugin class. Like `defclass`, but sets the metaclass and more?
Also accepts the name of the class to extend and enables itself automatically.
* `defextendable` (?): defines an extendable class. Like `defclass`.
* `enable`: enables a plugin by redefining the extendable class(es).
* `disable`: disables a plugin by redefining the extendable class(es).
* `enable-all`: enables all plugins for a given extendable class.
* `disable-all`: disables all plugins for a given extendable class.
* Some inspection APIs to list plugins, etc. Maybe functional interfaces for `defplugin` and the like.

Maybe `enable` and `disable` should optionally take the specific extendable class to
enable or disable? Or they dispatch on the metaclasses of their arguments? So:
* `(enable plugin)` - enable `plugin` on all compatible extendables
* `(disable extendable)` - disable all plugins on `extendable`
&c.

N.B.: it is expected this library is used with package local nicknames or similar,
not just `use`d. That way, the API functions are, e.g., `plug:enable` or similar.
