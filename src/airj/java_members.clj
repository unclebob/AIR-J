;; mutation-tested: 2026-03-11
(ns airj.java-members
  (:require [airj.java-types :as java-types])
  (:import (java.lang.reflect Constructor Field Method Modifier)))

(defn- signature-types
  [signature]
  {:params (mapv java-types/resolve-type (:params signature))
   :return-type (java-types/resolve-type (:return-type signature))})

(defn- load-class
  [class-name]
  (try
    (java-types/load-class class-name)
    (catch ClassNotFoundException _
      (throw (ex-info "Unknown Java class."
                      {:class-name class-name})))))

(defn- method-meta
  [class-name ^Method method member-id]
  {:kind :method
   :class-name class-name
   :member-id member-id
   :static? (Modifier/isStatic (.getModifiers method))
   :parameter-types (vec (.getParameterTypes method))
   :return-type (.getReturnType method)
   :declaring-class (.getDeclaringClass method)})

(defn- field-meta
  [class-name ^Field field field-name]
  {:kind :field
   :class-name class-name
   :field-name field-name
   :static? (Modifier/isStatic (.getModifiers field))
   :field-type (.getType field)
   :declaring-class (.getDeclaringClass field)})

(defn- constructor-meta
  [class-name ^Constructor ctor]
  {:kind :constructor
   :class-name class-name
   :parameter-types (vec (.getParameterTypes ctor))
   :declaring-class (.getDeclaringClass ctor)})

(defn resolve-constructor
  [class-name arg-types]
  (let [klass (load-class class-name)]
    (some (fn [^Constructor ctor]
            (when (and (= (count arg-types) (alength (.getParameterTypes ctor)))
                       (every? true? (map = arg-types (.getParameterTypes ctor))))
              (constructor-meta class-name ctor)))
          (.getConstructors klass))))

(defn resolve-static-method
  [class-name member-id signature]
  (let [klass (load-class class-name)
        {:keys [params return-type]} (signature-types signature)]
    (some (fn [^Method method]
            (when (and (= (name member-id) (.getName method))
                       (Modifier/isStatic (.getModifiers method))
                       (= (count params) (alength (.getParameterTypes method)))
                       (every? true? (map = params (.getParameterTypes method)))
                       (= return-type (.getReturnType method)))
              (method-meta class-name method member-id)))
          (.getMethods klass))))

(defn resolve-instance-method
  [receiver-type member-id signature]
  (let [klass (java-types/resolve-type receiver-type)
        class-name (second receiver-type)
        {:keys [params return-type]} (signature-types signature)]
    (some (fn [^Method method]
            (when (and (= (name member-id) (.getName method))
                       (not (Modifier/isStatic (.getModifiers method)))
                       (= (count params) (alength (.getParameterTypes method)))
                       (every? true? (map = params (.getParameterTypes method)))
                       (= return-type (.getReturnType method)))
              (method-meta class-name method member-id)))
          (.getMethods klass))))

(defn resolve-instance-field
  [receiver-type field-name field-type]
  (let [klass (java-types/resolve-type receiver-type)
        class-name (second receiver-type)
        resolved-type (java-types/resolve-type field-type)]
    (some (fn [^Field field]
            (when (and (= (name field-name) (.getName field))
                       (not (Modifier/isStatic (.getModifiers field)))
                       (= resolved-type (.getType field)))
              (field-meta class-name field field-name)))
          (.getFields klass))))
