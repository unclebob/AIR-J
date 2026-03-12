;; mutation-tested: 2026-03-11
(ns airj.java-effects)

(def ^:private pure-static-members
  #{['java.lang.Math 'abs [Integer/TYPE] Integer/TYPE]})

(def ^:private pure-instance-members
  #{['length [] Integer/TYPE]})

(defn- pure-static?
  [member]
  (contains? pure-static-members
             [(:class-name member)
              (:member-id member)
              (:parameter-types member)
              (:return-type member)]))

(defn- pure-instance?
  [member]
  (contains? pure-instance-members
             [(:member-id member)
              (:parameter-types member)
              (:return-type member)]))

(defn- call-effects
  [member]
  (if (pure-instance? member)
    #{}
    #{'Foreign.Throw}))

(defn- static-call-effects
  [member]
  (if (pure-static? member)
    #{}
    #{'Foreign.Throw}))

(defn- field-effects
  [member]
  (if (:writable? member)
    #{'State.Write}
    #{}))

(defn- method-effects
  [member]
  (if (:static? member)
    (static-call-effects member)
    (call-effects member)))

(defn member-effects
  [member]
  (case (:kind member)
    :constructor #{'Foreign.Throw}
    :method (method-effects member)
    :field (field-effects member)
    #{}))

(defn- write-field-effects
  [member]
  (member-effects (assoc member :writable? true)))

(defn interop-effects
  [expr member]
  (case (:op expr)
    :java-new #{'Foreign.Throw}
    :java-call (member-effects member)
    :java-static-call (member-effects member)
    :java-get-field #{}
    :java-set-field (write-field-effects member)
    #{}))
