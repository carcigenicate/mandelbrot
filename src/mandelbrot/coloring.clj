(ns mandelbrot.coloring
  (:require [helpers.general-helpers :as g])
  (:import [java.awt Color]))

(defn- cl [n]
  (g/clamp n 0 255))

(defn- wr [n]
  (g/wrap n 0 255))

(defn- co [^long r ^long g ^long b]
  (Color. r g b))

(defn lava [x y n]
  (co
    (cl (* n n n))
    (cl (* n n))
    (cl n)))

(defn exp [x y n]
  (co
    (wr (* x n 4))
    (wr (* y n 2))
    (wr (* n 2))))