;; thinkpad.lisp

(in-package :hardware)

;; http://www.thinkwiki.org/wiki/Thermal_Sensors#ThinkPad_T500

;; Index in "thermal"   Location
;; 1                    CPU (also via ACPI THM0)
;; 2                    ? (very similar to HDD smart temp)
;; 3                    ? (also via ACPI THM1)
;; 4                    n/a
;; 5                    Main Battery A
;; 6                    n/a (probably Second Battery A)
;; 7                    Main Battery B
;; 8                    n/a (probably Second Battery B)
;; 9                    ? (possibly (integrated) GPU)
;; 10                   ?
;; 11                   ? (possibly heatsink)
;; 12                   n/a
;; 13                   n/a
;; 14                   n/a
;; 15                   n/a
;; 16                   n/a

(defun thermal-data ()
  (let ((temps 
         (mapcar #'parse-integer 
                 (split-sequence #\Space 
                                 (subseq (first (slurp-lines "/proc/acpi/ibm/thermal")) 14)))))
    (values
      `((:cpu ,(first temps))
        (:hdd ,(second temps))
        (:thm1 ,(third temps))
        (:main-battery-a ,(fifth temps))
        (:main-battery-b ,(seventh temps)))
      temps)))

(defun thermal ()
  "Display the temperatures of the thermal sensors."
  (print-table (thermal-data)))
