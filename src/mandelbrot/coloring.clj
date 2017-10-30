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
  (co (wr (* x n 4))
      (wr (* y n 2))
      (wr (* n 2))))

(defn exp2 [x y n]
  (co (wr (* n 2))
      (wr (* x n 2))
      (wr (* y n 4))))

(defn dull [x y n]
  (co (cl (* x n 6))
      (cl (* y n 3))
      (cl (* n n 0.01))))

(defn tentacles [x y n]
  (co (cl (* n x y 2))
      (wr (* n n 0.5))
      (wr (* n 100))))

(defn crazy [x y n]
  (co (wr (* x n 5))
      (wr (* y n 3))
      (wr (* x y n n))))