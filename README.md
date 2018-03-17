cl-marshal
==========
Simple and fast marshalling of Lisp datastructures.
Convert any object into a string representation, put
it on a stream an revive it from there.
Only minimal changes required to make
your CLOS objects serializable. Actually you only need
to add 1 method per baseclass.

Just to make that clear: project is not dead - the
functionality is simply finished and working.

License
-------
MIT License. See included LICENSE file.

Usage
-----
###Basic Usage

For this ASDF needs to be installed and has to have access to marshal.asd.
Otherwise do a load *.lisp/tests-.lisp.

    (require :marshal)


Serialization of simple examples:

    $ (ms:marshal (list 1 2 3 "Foo" "Bar" (make-array '(3) :initial-contents '(a b c))))
    --> (:PCODE 1
              (:LIST 1 1 2 3 (:SIMPLE-STRING 2 "Foo") (:SIMPLE-STRING 3 "Bar")
              (:ARRAY 4 (3) T (A B C))))

Deserialization:

    $ (ms:unmarshal '(:PCODE 1
          (:LIST 1 1 2 3 (:SIMPLE-STRING 2 "Foo") (:SIMPLE-STRING 3 "Bar")
          (:ARRAY 4 (3) T (A B C)))))
    --> (1 2 3 "Foo" "Bar" #(A B C))

That means that a

    (ms:unmarshal (ms:marshal myobject))

returns a deep clone of myobject.




###Advanced Usage
Definition of a class

     (defclass ship ()
       ((name :initform "" :initarg :name :accessor name)
        (dimensions :initform '(:width 0 :length 0) :initarg :dimensions :accessor dimensions)
        (course :initform 0 :initarg :course :accessor course)
        (cruise :initform 0 :initarg :cruise :accessor cruise) ; shall be transient
        (dinghy :initform NIL :initarg :dinghy :accessor dinghy)) ; another ship -> ref
       (:documentation "A democlass. Some 'persistent slots', one transient.
      Some numbers, string, lists and object references."))


    (defparameter ark (make-instance 'ship :name "Ark" :course 360
                                :dimensions '(:width 30 :length 90)))
Let's try to serialize this:

    $ (ms:marshal ark)
    --> (:PCODE 1 NIL)

Actually nothing happens.

Next we define the method `class-persistent-slots` for this class. The method has to be defined
in the package `:marshal`.

    (defmethod ms:class-persistent-slots ((self ship))
      '(name dimensions course dinghy))
    --> #<STANDARD-METHOD MARSHAL:CLASS-PERSISTENT-SLOTS (SHIP) {1002B16B31}

Note that the slot `cruise` is not listed. Therefore it will not be serialized.

    $ (ms:marshal ark)
    -->  (:PCODE 1
                (:OBJECT 1 SHIP (:SIMPLE-STRING 2 "Ark") (:LIST 3 :WIDTH 30 :LENGTH 90) 360
	            (:LIST 4)))

Fine. Try a `(ms:unmarshal (ms:marshal ark))` and you will get a clone of the object ark.


Let's define some subclasses (yes, it's Lisp, we use multiple inheritance here).

    (defclass sailingship (ship)
      ((sailarea :initform 0 :initarg :sailarea :accessor sailarea))
      )

    (defclass motorship (ship)
      ((enginepower :initform 0 :initarg :enginepower :accessor enginepower))
    )

    (defclass motorsailor (motorship sailingship)
      ()
    )

Some instances:

     (defparameter ship2 (make-instance 'sailingship :name "Pinta" :course 270 :cruise 9
				   :dimensions '(:width 7 :length 21) :sailarea 400))
     (defparameter ship3 (make-instance 'motorship :name "Titanic" :course 320 :cruise 21
				   :dimensions '(:width 28 :length 269) :enginepower 51000))
     (defparameter ship4 (make-instance 'motorsailor  :name "Krusenstern" :course 180
				   :cruise 17.4 :dimensions '(:width 12 :length 82)
				   :sailarea 3400 :enginepower 2000))

Let's try

    $ (ms:marshal ship4)
    --> (:PCODE 1
               (:OBJECT 1 MOTORSAILOR (:SIMPLE-STRING 2 "Krusenstern")
                   (:LIST 3 :WIDTH 12 :LENGTH 82) 180 (:LIST 4)))

Note that the slots to be marshalled are determined by the function `ms:class-peristant-slots` of
the baseclass `ship`.

One last class and an instance. please note the backreference to
another ship. That's a circular reference.

    (defclass dinghy (ship)
      ((aboard :initform NIL :initarg :aboard :accessor aboard)) ; another ship -> circular ref
    )

    (defparameter ship5 (make-instance 'dinghy :name "Gig" :course 320 :cruise 5
				:dimensions '(:width 2 :length 6) :aboard ship4))
    (setf (dinghy ship4) ship5)

	$ (ms:marshal ship4)
	--> (:PCODE 1
               (:OBJECT 1 MOTORSAILOR (:SIMPLE-STRING 2 "Krusenstern")
                  (:LIST 3 :WIDTH 12 :LENGTH 82) 180
                  (:OBJECT 4 DINGHY (:SIMPLE-STRING 5 "Gig") (:LIST 6 :WIDTH 2 :LENGTH 6)
                    320 (:LIST 7))))

We see the reference to the dhingy (the "sub ship"), but not the
backreference. Simply, because so far the back link `aboard` is still transient.

    (defmethod ms:class-persistent-slots ((self dinghy))
      (append (call-next-method) '(aboard)))

 	$ (marshall ship4)
	--> (:PCODE 1
             (:OBJECT 1 MOTORSAILOR (:SIMPLE-STRING 2 "Krusenstern")
                 (:LIST 3 :WIDTH 12 :LENGTH 82) 180
                 (:OBJECT 4 DINGHY (:SIMPLE-STRING 5 "Gig") (:LIST 6 :WIDTH 2 :LENGTH 6)
                    320 (:LIST 7) (:REFERENCE 1))))

Brilliant!
References, circles et. are working regardless these are references from
and to lists, objects, hashtables, array etc.


Understanding the Implementation
--------------------------------
Everything is in the package :marshal, nickname :ms.

To enable the serialization of a class, you need to specialise the
method `ms:class-persistent-slots` for this class, or one of its
baseclasses. This method must reside in the package :marshal! It has
to return a list of slotnames. These slots will be serialized.

A call to `ms:marshal` on an object will generate the string (actually
a sexp) representation. It will try to find the method `ms:class-persistent-slots`
to see which slots have to be serialized.

A call to `ms:unmarshal` with sexp generated by a `ms:marshal` will
revive an object.

You may have different implementations of classes to be serialized
and deserialized. For examples different classes on the endpoints of
a net work connections. Or simply different classes as time passes
between the persistent storage of a serialization and its retrieval.
It is important to understand that the classes that are serialized and
the one of the object that will be deserialized need to have the same
name and need to have the same slotnames as listed in `ms:class-persistent-slots`.

If you define a method `ms:initialize-unmarshalled-instance` for your
class, then this method will be called in the end of the
deserialization process. This gives you the chance to initialize
transient slots, that were not serialized, or to do other
initialization tricks.

There is a function called 'coding-idiom', that defines the language of the
marshalling. The default vocabulary is quite verbose. In case you are
going to send the objects through a network, you may want to change
that to a shorter set of verbs. Well, I think there are better ways
to speed that up, e.g. by adding a nginx proxy with automaic gzip
compression in front of your lisp webserver. Anyway, you will find an
alternative, shorter implementation in `coding-idiom.lisp`, it is fairly
straight-forward.


Installation
------------

The most simple and recommended way to install cl-marshal is by using
[Quicklisp](http://www.quicklisp.org). If you installed Quicklisp a simple

    (ql:quickload :marshal)

will download the package and actually load it. You only need to do
this once per machine. Later a

    (require :marshal)

will be enough.



Alternatively you may get the code with:

    git clone git://github.com/wlbr/cl-marshal.git

Either you add this to your asdf repository, then you will only need
to do a `(require :marshal)` in your source.

Or, you may put the source in a subdirectory of your project and add
the file `marshal.asd` wih its full path to your own asdf definition.

Or, you may put the source in a subdirectory of your project and load
the file "marshal.asd" directly. After that a `(asdf:load-system "marshal")`
should be sufficient.

Or, as a kind of worst case, you simply do a direct `(load
<file>)` of the files `package.lisp`, `coding-idiom.lisp`, `marshal.lisp`,
and `unmarshal.lisp`.


Dependencies
---------------
None except for asdf.

xlunit for the unit tests only (the tests are not included in the asdf system).

Testing
----------
Tested with SBCL and CCL. No rocket science required, should run in
any environment.

A set of unit tests is included in tests.lisp.


Reporting problems
------------------
If you run into any trouble or find bugs, please report them via [the Github issue tracker](http://github.com/wlbr/cl-marshal/issues).

History
-------
First written as encode/decode for CLOS objects only during a diploma
thesis in '95. Major rework/enhancements during a research project
in the end of the '90s. Refactoring in 2011 when revisiting Lisp.

Contributors
------------
Written by Michael Wolber.
Major fixes and enhancements by Christoph Oechslein.
