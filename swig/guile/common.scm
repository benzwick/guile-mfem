;;;************************************************************************
;;;*common.scm
;;;*
;;;*     This file contains generic SWIG GOOPS classes for generated
;;;*     GOOPS file support
;;;*
;;;*     Modified from upstream SWIG to accept positional constructor
;;;*     arguments: (make <DenseMatrix> 2 3) instead of
;;;*     (make <DenseMatrix> #:args '(2 3)).
;;;************************************************************************

(define-module (Swig swigrun))

(define-module (Swig common)
  #:use-module (oop goops)
  #:use-module (Swig swigrun))

(define-class <swig-metaclass> (<class>)
  (new-function #:init-value #f))

(define-method (initialize (class <swig-metaclass>) initargs)
  (next-method)
  (slot-set! class 'new-function (get-keyword #:new-function initargs #f)))

(define-class <swig> ()
  (swig-smob #:init-value #f)
  #:metaclass <swig-metaclass>
)

(define-method (initialize (obj <swig>) initargs)
  ;; Supports three calling conventions:
  ;;   (make <C> #:init-smob smob)   — wrap existing C object
  ;;   (make <C> #:args '(x y))      — upstream SWIG keyword style
  ;;   (make <C> x y)                — positional args (our extension)
  (slot-set! obj 'swig-smob
    (let ((new-fn (slot-ref (class-of obj) 'new-function)))
      (cond
        ;; Keyword initargs: check for #:init-smob or #:args
        ((and (pair? initargs) (keyword? (car initargs)))
         (or (get-keyword #:init-smob initargs #f)
             (let ((ret (apply new-fn (get-keyword #:args initargs '()))))
               (if (slot-exists? ret 'swig-smob)
                 (slot-ref ret 'swig-smob)
                 ret))))
        ;; Positional args (or empty): pass directly to new-function
        (else
         (let ((ret (apply new-fn initargs)))
           (if (slot-exists? ret 'swig-smob)
             (slot-ref ret 'swig-smob)
             ret)))))))

(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define (display-pointer-address o file)
  ;; Don't fail if the function SWIG-PointerAddress is not present.
  (let ((address (false-if-exception (SWIG-PointerAddress o))))
    (if address
	(begin
	  (display " @ " file)
	  (display (number->string address 16) file)))))

(define-method (write (o <swig>) file)
  ;; We display _two_ addresses to show the object's identity:
  ;;  * first the address of the GOOPS proxy object,
  ;;  * second the pointer address.
  ;; The reason is that proxy objects are created and discarded on the
  ;; fly, so different proxy objects for the same C object will appear.
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display-pointer-address o file)
	  (display ">" file))
	(next-method))))

(define (swig-export! mod names)
  "Export NAMES from MOD for SWIG-generated GOOPS proxy modules.

Unlike Guile's standard export (which calls module-ensure-local-variable!
and can create #<undefined> locals that shadow imported generics), this
procedure finds the existing variable via module-variable — which checks
local bindings, imports, AND the custom binder — and adds it directly
to the public interface.  This is the approach used by G-Golf.

For names not yet bound (new generics created by define-method at load
time), module-ensure-local-variable! is used as a fallback."
  (let ((public-i (module-public-interface mod)))
    (for-each
      (lambda (name)
        (let ((var (module-variable mod name)))
          (module-add! public-i name
            (or var (module-ensure-local-variable! mod name)))))
      names)))

(define-syntax define-method/safe
  (syntax-rules ()
    "Like define-method but silently skips if a specializer class is unbound."
    ((_ formals body ...)
     (false-if-exception (define-method formals body ...)))))


(export <swig-metaclass> <swig> swig-export! define-method/safe)

;;; common.scm ends here
