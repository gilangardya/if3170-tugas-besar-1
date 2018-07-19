;;;
;;;
;;; SISTEM REKOMENDASI RESTORAN
;;;
;;;

; variabel global
(defglobal
  ?*dBumi* = 12742
  ?*plemen* = 0.017453292519943295)

; fungsi antara untuk distance
(deffunction aDistance (?lat1 ?long1 ?lat2 ?long2)
  (+ 0.5
     (/ (cos (* (- ?lat2 ?lat1) ?*plemen*)) (- 0 2))
     (* (cos (* ?lat1 ?*plemen*))
        (cos (* ?lat2 ?*plemen*))
        (/ (- 1 (cos (* (- ?long2 ?long1) ?*plemen*))) 2))))

; fungsi untuk menghitung jarak
(deffunction distance (?lat1 ?long1 ?lat2 ?long2)
  (* ?*dBumi* (asin (sqrt (aDistance ?lat1 ?long1 ?lat2 ?long2)))))

; fakta awal tentang restoran
(defrule initialrestaurant
=>
  (assert (print-sorted4))
  (assert (print-sorted32))
  (assert (print-sorted10))
  (assert (restaurant "A" isSmoker "True"))
  (assert (restaurant "A" minBudget 1000))
  (assert (restaurant "A" maxBudget 2000))
  (assert (restaurant "A" dresscode "casual"))
  (assert (restaurant "A" hasWifi "True"))
  (assert (restaurant "A" latitude -6.8922186))
  (assert (restaurant "A" longitude 107.5886173))

  (assert (restaurant "B" isSmoker "False"))
  (assert (restaurant "B" minBudget 1200))
  (assert (restaurant "B" maxBudget 2500))
  (assert (restaurant "B" dresscode "informal"))
  (assert (restaurant "B" hasWifi "True"))
  (assert (restaurant "B" latitude -6.224085))
  (assert (restaurant "B" longitude 106.7859815))

  (assert (restaurant "C" isSmoker "True"))
  (assert (restaurant "C" minBudget 2000)) 
  (assert (restaurant "C" maxBudget 4000))
  (assert (restaurant "C" dresscode "formal")) 
  (assert (restaurant "C" hasWifi "False"))
  (assert (restaurant "C" latitude -6.2145285))
  (assert (restaurant "C" longitude 106.8642591))

  (assert (restaurant "D" isSmoker "False"))
  (assert (restaurant "D" minBudget 500))
  (assert (restaurant "D" maxBudget 1400))
  (assert (restaurant "D" dresscode "formal"))
  (assert (restaurant "D" hasWifi "False"))
  (assert (restaurant "D" latitude -6.9005363))
  (assert (restaurant "D" longitude 107.6222191))

  (assert (restaurant "E" name RestaurantE))
  (assert (restaurant "E" isSmoker "True"))
  (assert (restaurant "E" minBudget 1000))
  (assert (restaurant "E" maxBudget 2000))
  (assert (restaurant "E" dresscode "casual"))
  (assert (restaurant "E" dresscode "informal"))
  (assert (restaurant "E" hasWifi "True"))
  (assert (restaurant "E" latitude -6.2055617))
  (assert (restaurant "E" longitude 106.8001597))

  (assert (restaurant "F" isSmoker "False"))
  (assert (restaurant "F" minBudget 2500))
  (assert (restaurant "F" maxBudget 5000))
  (assert (restaurant "F" dresscode "informal"))
  (assert (restaurant "F" hasWifi "True"))
  (assert (restaurant "F" latitude -6.9045679))
  (assert (restaurant "F" longitude 107.6399745))

  (assert (restaurant "G" isSmoker "True"))
  (assert (restaurant "G" minBudget 1300))
  (assert (restaurant "G" maxBudget 3000))
  (assert (restaurant "G" dresscode "casual"))
  (assert (restaurant "G" hasWifi "True"))
  (assert (restaurant "G" latitude -6.1881082))
  (assert (restaurant "G" longitude 106.7844409))

  (assert (restaurant "H" isSmoker "False"))
  (assert (restaurant "H" minBudget 400))
  (assert (restaurant "H" maxBudget 1000))
  (assert (restaurant "H" dresscode "informal"))
  (assert (restaurant "H" hasWifi "False"))
  (assert (restaurant "H" latitude -6.9525133))
  (assert (restaurant "H" longitude 107.6052906))

  (assert (restaurant "I" isSmoker "False"))
  (assert (restaurant "I" minBudget 750))
  (assert (restaurant "I" maxBudget 2200))
  (assert (restaurant "I" dresscode "informal"))
  (assert (restaurant "I" dresscode "casual"))
  (assert (restaurant "I" hasWifi "True"))
  (assert (restaurant "I" latitude -6.9586985))
  (assert (restaurant "I" longitude 107.7092281))

  (assert (restaurant "J" isSmoker "False"))
  (assert (restaurant "J" minBudget 1500))
  (assert (restaurant "J" maxBudget 2000))
  (assert (restaurant "J" dresscode "casual"))
  (assert (restaurant "J" hasWifi "True"))
  (assert (restaurant "J" latitude -6.2769732))
  (assert (restaurant "J" longitude 106.775133))
)
  
; input user  
(defrule userinput
=>
  (printout t "What is your name? ")
  (bind ?inputName (readline))
  
  (printout t "Do you smoke? (True,False) ")
  (bind ?inputSmoke (readline))
  
  (printout t "What is your minimum budget? [0-9999] ")
  (bind ?inputminBudget (readline))
  
  (printout t "What is your maximum budget? [0-9999] ")
  (bind ?inputmaxBudget (readline))
  
  (printout t "What clothes are you wearing? (casual, informal, formal) ")
  (bind ?inputDresscode (readline))
  
  (printout t "Do you want restaurant with Wi-Fi? (True,False) ")
  (bind ?inputWifi (readline))
  
  (printout t "What are your latitude coordinate? ")
  (bind ?inputLatitude (readline))
  
  (printout t "What are your longitude coordinate? ")
  (bind ?inputLongitude (readline))
  
  (assert 	(user name ?inputName)
        	(user isSmoker ?inputSmoke) 
            (user minBudget ?inputminBudget) 
            (user maxBudget ?inputmaxBudget) 
            (user dresscode ?inputDresscode) 
            (user hasWifi ?inputWifi) 
            (user latitude ?inputLatitude) 
            (user longitude ?inputLongitude))
)

; preprocessing minimum budget
(defrule prepminbudget1
	
	?f <- (user minBudget "")
=>
	(retract ?f)
	(assert (user minBudget 0))
)
(defrule prepminbudget2
	
	?f <- (user minBudget ?n)
	(test (eq (type ?n) STRING))
=>
	(retract ?f)
	(assert (user minBudget (string-to-field ?n)))
)

; preprocessing maximum budget
(defrule prepmaxbudget1
	
	?f <- (user maxBudget "")
=>
	(retract ?f)
	(assert (user maxBudget 9999))
)
(defrule prepmaxbudget2
	
	?f <- (user maxBudget ?n)
	(test (eq (type ?n) STRING))
=>
	(retract ?f)
	(assert (user maxBudget (string-to-field ?n)))
)

; preprocessing latitude
(defrule preplatitude1
	
	?f <- (user latitude "")
=>
	(retract ?f)
	(assert (user latitude -6.890621))
)
(defrule preplatitude2
	
	?f <- (user latitude ?n)
	(test (eq (type ?n) STRING))
=>
	(retract ?f)
	(assert (user latitude (string-to-field ?n)))
)

; preprocessing longitude
(defrule preplongitude1
	
	?f <- (user longitude "")
=>
	(retract ?f)
	(assert (user longitude 107.609543))
)
(defrule preplongitude2
	
	?f <- (user longitude ?n)
	(test (eq (type ?n) STRING))
=>
	(retract ?f)
	(assert (user longitude (string-to-field ?n)))
)

; inisialisasi checklist
(defrule initialchecklist
	(restaurant ?name isSmoker ?)
=>
  (assert 	(checklist ?name cekIsSmoker "False")
  			(checklist ?name cekBudget "False")
  			(checklist ?name cekDresscode "False")
  			(checklist ?name cekHasWifi "False"))
)

; inisialisasi skor
(defrule initialscore
	(restaurant ?rName latitude ?rLatitude)
	(restaurant ?rName longitude ?rLongitude)
  	(user latitude ?uLatitude)
  	(user longitude ?uLongitude)
  	(test (or (eq (type ?uLatitude) FLOAT) (eq (type ?uLatitude) INTEGER)))
	(test (or (eq (type ?uLongitude) FLOAT) (eq (type ?uLongitude) INTEGER)))
=>
  (assert	(score ?rName point 0)
  			(score ?rName jarak (distance ?rLatitude ?rLongitude ?uLatitude ?uLongitude)))
)

; penilaian kriteria isSmoker
(defrule checksmoke
	(user isSmoker ?userSmoker)
	(or (restaurant ?restaurantName isSmoker ?userSmoker) (test (eq ?userSmoker "")))
	?fs <- (score ?restaurantName point ?n)
	?fc <- (checklist ?restaurantName cekIsSmoker "False")
=>
	(retract ?fs)
	(retract ?fc)
	(assert (score ?restaurantName point (+ 1 ?n)))
	(assert (checklist ?restaurantName cekIsSmoker "True"))
)

; penilaian kriteria minBudget dan maxBudget
(defrule checkbudget
	(user minBudget ?userMinBudget)
	(user maxBudget ?userMaxBudget)
	(restaurant ?restaurantName minBudget ?restaurantMinBudget)
	(restaurant ?restaurantName maxBudget ?restaurantMaxBudget)
	(test (or (eq (type ?userMinBudget) FLOAT) (eq (type ?userMinBudget) INTEGER)))
	(test (or (eq (type ?userMaxBudget) FLOAT) (eq (type ?userMaxBudget) INTEGER)))
	(test (and (>= ?userMaxBudget ?restaurantMinBudget) (<= ?userMinBudget ?restaurantMaxBudget)))
	?fs <- (score ?restaurantName point ?n)
	?fc <- (checklist ?restaurantName cekBudget "False")
=>
	(retract ?fs)
	(retract ?fc)
	(assert (score ?restaurantName point (+ 1 ?n)))
	(assert (checklist ?restaurantName cekBudget "True"))
)

; penilaian kriteria dresscode
(defrule checkdresscode
	(user dresscode ?userDresscode)
	(or (restaurant ?restaurantName dresscode ?userDresscode) (test (eq ?userDresscode "")))
	?fs <- (score ?restaurantName point ?n)
	?fc <- (checklist ?restaurantName cekDresscode "False")
=>
	(retract ?fs)
	(retract ?fc)
	(assert (score ?restaurantName point (+ 1 ?n)))
	(assert (checklist ?restaurantName cekDresscode "True"))
)

; penilaian kriteria hasWifi
(defrule checkwifi
	(user hasWifi ?userWifi)
	(or (restaurant ?restaurantName hasWifi ?userWifi) (test (eq ?userWifi "")))
	?fs <- (score ?restaurantName point ?n)
	?fc <- (checklist ?restaurantName cekHasWifi "False")
=>
	(retract ?fs)
	(retract ?fc)
	(assert (score ?restaurantName point (+ 1 ?n)))
	(assert (checklist ?restaurantName cekHasWifi "True"))
)

(defrule assert-unprinted4
  (declare (salience -100))
	(print-sorted4)
	(score ?nama point 4)
=>
	(assert (unprinted4 ?nama)))

(defrule assert-unprinted32
  (declare (salience -200))
  (print-sorted32)
  (score ?nama point 3|2)
=>
  (assert (unprinted32 ?nama)))

(defrule assert-unprinted10
  (declare (salience -300))
  (print-sorted10)
  (score ?nama point 1|0)
=>
  (assert (unprinted10 ?nama)))


(defrule retract-print-sorted
  (declare (salience -10))
  ?f <- (print-sorted)
  =>
  (retract ?f))

(defrule print-terdekat
	(not (print-sorted4))
	?u <- (unprinted4 ?nama)
	(score ?nama point ?point)
	(score ?nama jarak ?jarak)
	(forall (and (unprinted4 ?n) (score ?n jarak ?r))
			(test (>= ?r ?jarak)))
=>
	(retract ?u)
	(printout t ?nama " " ?jarak " " ?point crlf))

(defrule print-terdekat3
	(not (print-sorted32))
	?u <- (unprinted32 ?nama)
	(score ?nama point ?point)
	(score ?nama jarak ?jarak)
	(forall (and (unprinted32 ?n) (score ?n jarak ?r))
			(test (>= ?r ?jarak)))
=>
	(retract ?u)
	(printout t ?nama " " ?jarak " " ?point crlf))

(defrule print-terdekat0
	(not (print-sorted10))
	?u <- (unprinted10 ?nama)
	(score ?nama point ?point)
	(score ?nama jarak ?jarak)
	(forall (and (unprinted10 ?n) (score ?n jarak ?r))
			(test (>= ?r ?jarak)))
=>
	(retract ?u)
	(printout t ?nama " " ?jarak " " ?point crlf))