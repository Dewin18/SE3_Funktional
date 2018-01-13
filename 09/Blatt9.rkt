#lang swindle
(require swindle/setf
         swindle/misc)

;;;1
;;1

;this method is for 1.2 but needs to be here
(defgeneric cite ((LiteraturBeitrag))
  :combination generic-begin-combination)


(defclass* wissenschaftlicheVeröffentlichung ()
  (Key :initvalue 0
       :reader K
       :accesor cite
       :initarg :key)
  (Autor :initvalue ""
         :reader A
         :accesor cite
         :initarg :author)
  (PublishingYear :initvalue 0
                  :reader PY
                  :accesor cite
                  :initarg :year)
  (Titel :initvalue ""
         :reader T
         :accesor cite
         :initarg :titel))
  


(defclass* Buch (wissenschaftlicheVeröffentlichung)
  (PublisherCompany :initvalue ""
             :reader PC
             :accesor cite
             :initarg :pc)
  (PublishPlace :initvalue ""
                :reader PP
                :accesor cite
                :initarg :pp)
  (Series :initvalue ""
          :reader S
          :accesor cite
          :initarg :series)
  (SerieNumber :initvalue 1
               :reader SN
               :accesor cite
               :initarg :sn))
(define Buch1 (make Buch :Key 1  :year 1790 :titel "Nessie" :pc "Minority-Verlag" :pp "Inverness" :series "Die Besondere Biographie" :sn 1))
;(println  (T Buch1))
;(println (A Buch1))

(defclass* Sammelband (Buch)
  (PuplisherName :initvalue ""
                 :reader PN
                 :initarg :pn)
  (PageNumber :initvalue 0
              :reader PageN
              :initarg :pagen))
(define Sammellband1 (make Sammelband :Key 1 :author "Adam D" :year 1979 :titel "The Hitchhiker's Guide to the Galaxy" :pc "Galactic Press" :pp "Vega-System, 3rd planet" :series "Travel in Style" :sn 5 :pagen 500))
;(println (T Sammellband1))
;(println (PN Sammellband1))
;(println (PageN Sammellband1))

(defclass* Zeitschriftenartikel ()
  (Name :initvalue ""
        :reader N
        :initarg :n)
  (Volume :initvalue 0
          :reader V
          :initarg :v)
  (Magzine :initvalue ""
           :reader M
           :initarg :m)
  (PublishingMonth :initvalue 0
                   :reader PM
                   :initarg :pm))
(define Zeitschriftenartikel1 (make Zeitschriftenartikel :n "Zeitmaschinen leicht gemacht. Heimwerkerpraxis für Anfänger" :v 3200 :m 500 ))
;(println (N Zeitschriftenartikel1))
;(println (PM Zeitschriftenartikel1))

;;2

(defmethod cite ((b Buch))
  (print (string-append  (number->string (K b)) " "(A b) " (" (number->string (PY b)) "). " (T b) " Band " (number->string (SN b)) " der Reihe: " (S b)". "(PC b)", "(PP b) )))
(defmethod cite ((w wissenschaftlicheVeröffentlichung))
  (print (string-append  (number->string (K w)) " "(A w) " (" (number->string (PY w)) "). " (T w) )))
(defmethod cite ((s Sammelband))
  (print (string-append  (number->string (K s)) " "(A s) " (" (number->string (PY s)) "). " (T s) " Band " (number->string (SN s)) " der Reihe: " (S s)". "(PC s)", "(PP s) ". " (PN s)", " (number->string (PageN s)))))
(defmethod cite ((z Zeitschriftenartikel))
  (print (string-append (number->string (V z)) ". "(N z)", " (number->string (V z)) ". " (number->string (M z)) ", " (number->string (PM z)) )))
;(cite Buch1)
;(cite Zeitschriftenartikel1)

;;3
#|
Ergänzungsmethoden are adding helping methods to the primary method of the overclass these helping methods are used before, after or around the primary method.
This is another way to specialise a class for inherited classes.
We could use this for if we had a class wich uses  alot from another class wich is inherited and after the lower class did everyhting the higher class did we want to also do something else so we use :after
|#
;;;2
;;1,2,3
;1

;we want to know wich savingtyp it is and for that a simple combination of the savingtyps is sufficent
(defgeneric speicherTyp ((Speichermedien))
  :combination generic-begin-combination)
;we want to know the higest reading speed so we use max
(defgeneric geschwindigkeit ((Speichermedien))
  :combination generic-max-combination)
;we want to know the highest capazity so we use max
(defgeneric kapazität ((Speichermedien))
  :combination generic-max-combination)
;we want to know the average living duration a really good option is lacking therefore we use begin wich kinda works 
(defgeneric lebensdauer ((Speichermedien))
  :combination generic-begin-combination)
;we want to know the mobility and begin is suffiient for that
(defgeneric mobilität ((Speichermedien))
  :combination generic-begin-combination)

(defclass Speichermedien ()
  (Speichertyp :reader S :accessor speicherTyp)
  (Maximalgeschwindigkeit :reader MS :accessor geschwindigkeit)
  (Kapazität :reader K :accessor kapazität)
  (durschnitlicheLebensdauer :reader dS :accessor lebensdauer)
  (Mobilität :reader Mob :accessor mobilität))

(defclass MagnetischeSpeichermedien (Speichermedien)
  (Speichertyp :initvalue "magnetic"))

(defclass festVerbauteHDD (MagnetischeSpeichermedien))

(defclass herausnehmbareDisketten (MagnetischeSpeichermedien))

(defclass optischeSpeicherMedienCdDVD (Speichermedien)
  (Speichertyp :initvalue "Optisch"))

(defclass Halbleiterspeicher (Speichermedien)
  (Speichertyp :initvalue "Halbleiter"))

(defclass festVerbauteSSDoderRAM (Halbleiterspeicher))
(defclass USBSticks (Halbleiterspeicher))
;2
(defclass MagnetoOpticalDisc (MagnetischeSpeichermedien optischeSpeicherMedienCdDVD))
(defclass SSHD (Halbleiterspeicher MagnetischeSpeichermedien))
(defclass Bankkarte (MagnetischeSpeichermedien Halbleiterspeicher))


;;3
(defmethod speicherTyp ((m MagnetischeSpeichermedien))
  (print (S m)))
(defmethod speicherTyp ((o optischeSpeicherMedienCdDVD))
  (print (S o)))
(defmethod speicherTyp ((h Halbleiterspeicher))
  (print (S h)))

;(define a (make festVerbauteHDD))
;(speicherTyp a)