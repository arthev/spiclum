# SPICLUM

System Prevalence In Common Lisp Using MOP, pronounced however spiculum's pronounced. In case you'd like to disregard the collective wisdom of roughly sixty years of database usage, and instead keep an object store in memory, serialize to and from text files, and rely on as much automagic as possible.

## Example

```common-lisp
CL-USER> (ql:quickload :spiclum) ;; local quicklisp path
(:SPICLUM)
CL-USER> (spiclum:load-world :directory "/home/arthur/tmp" :name "spiclum-example")
0
CL-USER> (spiclum:defpclass some-class ()
           ((some-slot :initarg :some-slot
                       :accessor some-slot
                       :key :unique
                       :equality #'equalp)))
         (defmethod print-object ((obj some-class) out)
           (print-unreadable-object (obj out :type t)
             (format out "some-slot: ~s" (some-slot obj))))
#<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (SOME-CLASS T) {1003CB43D3}>
CL-USER> (make-instance 'some-class :some-slot 12)
#<SOME-CLASS some-slot: 12>
CL-USER> (make-instance 'some-class :some-slot 20)
#<SOME-CLASS some-slot: 20>
CL-USER> (spiclum:query :class some-class
                        :where (some-slot 12))
(#<SOME-CLASS some-slot: 12>)
CL-USER> (spiclum:query :class some-class
                        :where (:or (some-slot 0)
                                    (some-slot (lambda (x)
                                                 (< 13 x 21)))))
(#<SOME-CLASS some-slot: 20>)
CL-USER> ;; Let's do a slime-restart-inferior-lisp
CL-USER> (ql:quickload :spiclum) ;; local quicklisp path
(:SPICLUM)
CL-USER> (spiclum:load-world :directory "/home/arthur/tmp" :name "spiclum-example")
2
CL-USER> (find-class 'some-class)
#<SPICLUM:PREVALENCE-CLASS COMMON-LISP-USER::SOME-CLASS>
CL-USER> (c2mop:compute-applicable-methods-using-classes
          #'print-object
          (list (find-class 'some-class)
                (find-class 'stream)))
(#<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (STANDARD-OBJECT T) {100052E143}>
 #<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (T T) {100052E763}>)
T
CL-USER> (defmethod print-object ((obj some-class) out)
           (print-unreadable-object (obj out :type t)
             (format out "some-slot: ~s" (some-slot obj))))
#<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (SOME-CLASS T) {1003A36E53}>
CL-USER> (spiclum:query :class some-class)
(#<SOME-CLASS some-slot: 12> #<SOME-CLASS some-slot: 20>)
```

## Portability

The following implementations are supported:

* SBCL 2.0.2+

There are only three places where reader conditionals appear, though. So the amount of work to port SPICLUM to other implementations is presumably low. But it's a presumption.

## Design

SPICLUM exists because I wanted data persistence without having to fiddle with database installations/setups (for ORMs), and the existent libraries for object prevalence in Lisp seemed to require more explicit actions than I wanted. Why create transaction objects when the metaobject protocol defines actions for updating the state of objects, and we can simply make use of those *as* the transactional actions. As such, SPICLUM treats the following generic functions as transactional actions: `slot-makunbound-using-class`, `(setf slot-value-using-class)`, `make-instance`, `ensure-class-using-metaclass`, `change-class`, `reinitialize-instance`. That *should* be all normal ways to change an instance or a class.

Note that the above list of generic functions mentions `ensure-class-using-metaclass`, rather than `ensure-class-using-class`. I find the design of `ensure-class-using-class` deficient - there's some rambling about this [starting here](https://abstractaway.com/the-art-of-the-metaobject-protocol-and-ensure-class-using-class/). Basically, SPICLUM replaces `ensure-class-using-class` with `ensure-class-using-metaclass` which makes it possible to specialize on the metaclass (by calling `ensure-class-using-metaclass` with a prototype instance of the relevant metaclass).

Since mutable values as slot-values can be updated without hitting any of the above "protocol points", ensuring all changes get transactionalized requires some more `setf`ing (of the slot-values) than would otherwise be ideal.

#### Undefined Behaviour, Restrictions, Limitations, etc.

Saving a world serializes both class definitions and instances. This is to ensure the coherency between the instantiation forms and the class definitions. To this end, class redefinitions also get committed to the transaction log.

Since preserving referentiality in general would require analysing the whole world for each and every transactive action, SPICLUM doesn't preserve general referentiality. It *does* preserve referentiality between prevalence-objects themselves, though. (So e.g. two prevalence-objects that both hold references to a list will, when the world is reloaded, each refer to a distinct list with similar elements instead.)

If a subclass of prevalence-object takes other initargs than those that match its slot definitions, the application programmer must supply a custom serialization method for that class.

A programmer calling make-instances-obsolete on a prevalence-class is undefined behaviour. (The calls that happen through defclass or ensure-class-using-metaclass are well-defined.)

Specifying a class-allocated slot as a key is an error (i.e. a class-allocated slot must have `:key nil`).

Prevalence-object defines two slots: mutating these is a violation.

Non-prevalence-object mixins/superclasses (for prevalence-objects) aren't supported.

The `prevalence-class` metaclass and the `prevalence-object` class are meant to work together. Having classes of metaclass `prevalence-class`, but which don't subclass `prevalence-object`, or vise versa, is undefined behaviour.

SPICLUM isn't safe in the presence of interrupts. (As far as I know, interrupts are non-portable. I *think* SPICLUM could be made decently safe in the face of interrupts by adjusting some of the object-store actions.)

Deleting prevalence-objects is given a single interface - `delete-object` - which removes a single prevalence-object from the object-store. Pruning references to the deleted object is left to the application programmer: locating them without knowing the application rules would require analyzing the whole world, and there's no natural semantic for what to do when pruning either. (Suppose we have a list of prevalence-objects and one of them are deleted, what should the new list look like? One element shorter? The deleted object replaced by a nil? The deleted object replaced by a special deleted-object constant?)

#### Unsupported Slot Values

Not all types of slot-values (or indirect values e.g. elements of hash-tables, lists, etc.) are supported for prevalence-objects (but note that support can be added by writing appropriate serialize-object methods):

* Anonymous classes - can't locate class objects (and using names for keys becomes tricky)
* Displaced arrays - we'd need to analyze the whole world to keep track of them
* Circular data (although circular relations between prevalence-objects *is* supported)
* CLOS instances which aren't prevalence-objects - because of the lack of general referentiality
* Closures - no portable access to source code or lexical environment
* Conditions - same general problem as structs
* Functions (named) - no good portable way to access names of functions, AFAIK. Have a look at the lax requirements of `function-lambda-expression`.
* Packages - not investigated yet, but not implemented
* Pathnames - should be decently straightforward, but not implemented
* Readtables - they contain functions
* Streams - not investigated yet, but not implemented
* Structs - they have terrible (portable) introspection


#### Assorted Observations

SPICLUM uses bordeaux-threads since it seems to be the de-facto standard. At the time of implementation, I found two downsides to bordeaux-threads: 1) No way to portably access the name of a lock 2) `acquire-recursive-lock` wasn't supported for SBCL. There's a (short) bordeaux-threads section in language-utils.lisp.

SPICLUM has no support for distributed systems.

## Installation

From source. E.g. clone the repo and place the folder in your local quicklisp search path.

## License

... [TODO]

## Documentation

###### macro: defpclass (class-name superclasses slot-specifiers &rest class-options)

Convenience macro that inserts the `prevalence-class` metaclass and the `prevalence-object` superclass into a defclass form. Identical to a `defclass` form, except `defpclass` doesn't accept a `:metaclass` class-option, and the slot-specifiers for `defpclass` accept a `:key` and `:equality` arg.

###### function: delete-object (obj)

Accepts a prevalence-object and removes it from the object-store. Note that pruning all references to the prevalence-object in question, including from other prevalence-objects, is left to the application programmer.

###### class: prevalence-class

Metaclass for prevalence-objects. Some methods, e.g. `c2mop:slot-value-using-class`, specialize on `prevalence-class` as part of the automagic object-store and serializations.

In addition, the slots of instances of `prevalence-class` are instances of `keyable-slot` rather than (directly) `c2mop:standard-slot-definition`. A `keyable-slot` extends `c2mop:standard-slot-definition` with a `key` and `equality` slot.

`key` must be one of `'(nil :unique :class-unique :index)`. `nil` means the slot in question is *not* used as an index. `:unique` means it's a unique key (for all subclasses that share the slot definition). `:index` means the slot is used for indexing without any uniqueness restraint. `:class-unique` means the slot is a unique key, but the uniqueness restraint applies at the level of each direct class, hence different subclasses can each have an instance wih a particular unique value for the slot.

`equality` must be one of the hash-table test functions.

###### class: prevalence-object

Superclass for all prevalence-objects. Some of the intercessory methods necessary for SPICLUM's automagic (e.g. `change-class`) didn't specialize on the metaclass, hence the necessity of this superclass instead. `prevalence-object` also defines some slots used by the object-store.

###### function: call-query (&key select class strict where)

Functional seam for the `query` macro. Made available in case someone wants to e.g. construe query filters programmatically. (Compared with `query`: Supply the class object instead of the class name, and supply a cons structure that matches the description of `where`.)

###### macro: query (&key (select :all) class (strict nil) where)

Macro interface for selecting objects from the object-store, which defines a little query language. Given the differences between an in-memory object-store and a RDBMS, no effort was made to ape any existent query language.

`select` must be either `:all` or `:the`: If you want a set of all prevalence-objects that meet the selection criteria, use `:all`. If you want the first matching prevalence-object (in the expectation only one object will match), use `:the`.

`class` must be the symbol that names a class.

`strict` must be either `nil` or `t`. If `t`, we only query for direct members of the class in question. If `nil`, we also query for members of all the subclasses.

`where` specifies a filter:

```
<where> ::= nil | <filter>
<filter> ::= <simple-filter> | <compound-filter>
<compound-filter> ::= (:and <multi-filter>) | (:or <multi-filter>) | (:not <filter>)
<multi-filter> ::= <filter> | <filter> <multi-filter>
<simple-filter> ::= (<slot-name> <comparison>)
<comparison> ::= <comparison-value> | <comparison-function>
<slot-name> ::= the symbol which names a slot of the relevant `class`
<value> ::= a value tested for equality against the relevant slot-value, using the slot's equality function
<comparison-function> ::= an arbitrary unary predicate
```

See query-tests.lisp for some examples.

###### function: load-world (&key directory name)

Loads a world and transaction log (or creates empty ones if necessary), and initializes the object-store. `directory` and `name` are required.

###### function: save-world (&key directory name)

Saves a new world, so that the world can be loaded directly rather than through re-enacting the transaction log. If `directory` or `name` (or both) are `nil`, SPICLUM saves to appropriate files as per the `directory` and `name` supplied to `load-world` when SPICLUM was initialized. "World" means the state of the object-store, including current class definitions.

###### condition: non-unique-unique-keys

Presumed self-explanatory.

###### condition: prevalence-breach

Presumed self-explanatory.

###### condition: removing-nonexistant-entry

Presumed self-explanatory.

###### generic function: acceptable-persistent-slot-value-type-p (value)

Predicate to decide whether we allow `value` as a slot-value for a prevalence-object. If extending the allowable values ([see](#unsupported-slot-values)) or adding custom checks, you might need to define new methods on it. The default method checks for the existence of an applicable `serialize-object` method for `value`.

###### generic function: force

Prevalence-object references are serialized as lazy lookups, which get converted into references through the use of `force`. If you extend the allowable slot-values ([see](#unsupported-slot-values)) to another compound-value (e.g. structs) that can have prevalence-objects as one of those values, you need to write a `force` method that ensures the lazy lookups get `force`d.

E.g. (in pseudo-code):
```common lisp
(defmethod force ((struct struct))
  (for-each-slot (setf slot (force slot))))
```

###### generic function: serialize-object

If you extend the allowable slot-values ([see](#unsupported-slot-values)), you will need to write a serialization for it as a `serialize-object` method. The serialization must be as lisp code which construes an equivalent object (for whatever value of "equivalent" you deem appropriate) when `load`ing the world/transaction log.