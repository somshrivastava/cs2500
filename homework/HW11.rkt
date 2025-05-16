;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or not Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.

;;! HW11

;; In the era of the Internet of Things (IoT), smart home systems play an important role
;; in enhancing convenience, security, and energy efficiency. Efficient data processing
;; and algorithms are essential for optimizing device performance, as they ensure user
;; satisfaction and maintain system security. As a software developer working
;; on smart home systems, you are tasked with implementing algorithms that handle large
;; datasets, perform real-time data analysis, and maintain system performance.

;; Consider the following data definition for a SmartHomeDevice:

(define-struct device [name energy-consumption])
;; A SmartHomeDevice is a (make-device String Number)
;; Representing a smart home device
;; - name is the name of the device
;; - energy-consumption is the energy consumption of the device in watts

(define D1 (make-device "Thermostat" 150))
(define D2 (make-device "Light Bulb" 60))
(define D3 (make-device "Security Camera" 100))
(define D4 (make-device "Security Camera" 30))
(define D5 (make-device "Security Camera" 0))

(define (device-temp d)
  (... (device-name d) ... (device-energy-consumption d) ...))

;;! Problem 1

;; Implement a function `devices-possible` that takes a list of devices and a
;; total power budget (in watts) and returns a prefix of the list whose total
;; power usage is as close to the power budget as possible without exceeding it.

;;!! IMPORTANT: Write your response BELOW this line:

;; devices-possible : [List-of Device] Number -> [List-of Device]
;; takes a list of devices and a total power budget (in watts) and returns a prefix of the list
;; whose total power usage is as close to the power budget as possible without exceeding it
(define (devices-possible devices budget)
  (local [
          (define (helper lst prefix sum)
            (cond
              [(empty? lst) prefix]
              [else
               (if (<= (+ sum (device-energy-consumption (first lst))) budget)
                   (helper (rest lst)
                         (append prefix (list (first lst)))
                         (+ sum (device-energy-consumption (first lst))))
                   prefix)]))]
    (helper devices '() 0)))

;; Test Cases
(check-expect (devices-possible (list D1 D2 D3) 200) (list D1)) 
(check-expect (devices-possible (list D1 D2 D3) 50) '())
(check-expect (devices-possible (list D1 D2 D3 D4) 90) '())


;;! Problem 2

;; Design a function total-energy, that returns the total energy consumed by a
;; list of devices. If 3 of the devices have an energy consumption of 0
;; (indicating the devices are inactive), the entire function should immediately
;; return the total energy consumption up to the point where the 3rd inactive device was encountered.

;;!! IMPORTANT: Write your response BELOW this line:

;; total-energy : [List-of Device] -> Number
;; that returns the total energy consumed by a list of devices, if 3 of the devices have an energy
;; consumption of 0 (indicating the devices are inactive), the entire function should immediately
;; return the total energy consumption up to the point where the 3rd inactive device was encountered
(define (total-energy devices)
  (local [(define (helper lst inactive total)
            (cond [(empty? lst) total]
                  [(>= inactive 3) total]
                  [(= (device-energy-consumption (first lst)) 0) (helper (rest lst)
                                                                              (add1 inactive)
                                                                              total)]
                  [else (helper (rest lst) inactive (+ (device-energy-consumption (first lst))
                                                       total))]))]
    (helper devices 0 0)))

;; Test Cases
(check-expect (total-energy (list D1 D2 D3)) 310) 
(check-expect (total-energy (list D1 D2 D5)) 210) 
(check-expect (total-energy (list D5 D1 D2 D5 D3 D5 D4)) 310)

;;! Problem 3

;;! Part A

;; Design the data definition Circuit that can represent either outlets, wires
;; (that lead to another Circuit, indicated with an arrow in diagram), junctions
;; (that have three Circuits coming out of them), or a dead end.

;; An example that you would want to be able to represent is (wire abbreviated w
;; in places):

;; --- wire ---> |junction|
;;                /  |  \
;;               w   w   w
;;              /    |    \
;;             \/    |    \/
;;          outlet   |   outlet
;;                  \|/
;;                deadend

;;!! IMPORTANT: Write your response BELOW this line:

;; A Circuit is one of:
;; - "outlet"
;; - "deadend"
;; - (make-wire Circuit)
;; - (make-junction Circuit Circuit Circuit)
;; Interpretation: A circuit is where:
;; - "outlet" is an oulet
;; - "deadend" is a deadend
;; - (make-wire to) is a wire that leads to another Circuit
;; - (make-junction c1 c2 c3) is a junction has three Circuits coming out

(define-struct wire [c])
(define-struct junction [c1 c2 c3])

(define OUTLET "outlet")
(define DEADEND "deadend")
(define WIRE1 (make-wire OUTLET))   
(define WIRE2 (make-wire DEADEND))  
(define WIRE3 (make-wire WIRE1))    
(define JUNCTION1 (make-junction WIRE1 WIRE2 WIRE3)) 
(define CIRCUIT1 (make-junction WIRE1 WIRE3 (make-wire JUNCTION1))) 

; circuit-temp : Circuit -> ?
(define (circuit-temp c)
  (cond
    [(string=? c "outlet") ...]
    [(string=? c "deadend") ...]
    [(wire? c) (circuit-temp (wire-c c))]
    [(junction? c) (... (circuit-temp (junction-c1 c)) ...
                    ... (circuit-temp (junction-c2 c)) ...
                    ... (circuit-temp (junction-c3 c)))]))

;;! Part B

;; Define an interesting example of your data definition, as the constant `CIRCUIT-EX`.
;; NOTE: The autograder for Problem 4 will fail if you submit without defining `CIRCUIT-EX`.

;;!! IMPORTANT: Write your response BELOW this line:

(define CIRCUIT-EX CIRCUIT1)

;;! Problem 4

;; Design a function, length-to-wires, that given a Circuit, returns a list of
;; numbers that represent the length, in number of wires, to each of the outlets
;; in the Circuit.

;;!! IMPORTANT: Write your response BELOW this line:

;; length-to-wires : Circuit -> [List-of Number]
;; given a Circuit, returns a list of numbers that represent the length, in number of wires,
;; to each of the outlets in the Circuit
(define (length-to-wires circuit)
  (cond
    [(string? circuit) (if (string=? circuit "outlet") (list 0) '())]
    [(wire? circuit) (map (lambda (x) (add1 x)) (length-to-wires (wire-c circuit)))]
    [(junction? circuit) (append (length-to-wires (junction-c1 circuit))
                    (length-to-wires (junction-c2 circuit))
                    (length-to-wires (junction-c3 circuit)))]))

;; Test Cases
(check-expect (length-to-wires OUTLET) (list 0))
(check-expect (length-to-wires DEADEND) '()) 
(check-expect (length-to-wires WIRE1) (list 1))
(check-expect (length-to-wires WIRE2) '()) 
(check-expect (length-to-wires WIRE3) (list 2))
(check-expect (length-to-wires JUNCTION1) (list 1 2))
(check-expect (length-to-wires CIRCUIT-EX) (list 1 2 2 3)) 

;;! Problem 5
;; INTERPRETIVE QUESTION
;;
;; While smart home devices are often sold because they can either increase
;; convenience or sometimes decrease total energy use by "intelligently" turning
;; off, there are several new challenges introduced by their use:
;;
;; 1. Privacy / surveillance concerns. Many devices transmit their data to
;; centralized data centers, and if those are compromised, anything from the
;; voice recordings from smart speakers to videos from baby monitors could be
;; made available. This has been the subject of federal law in the United States,
;; with Internet of Things Cybersecurity Improvement Act of 2020 setting minimum
;; security standards for such devices if used by the federal government.
;;
;; 2. Increase of "electronic waste" or e-waste: many devices only work when in
;; contact with software running on remote servers, and if the company changes
;; the software or goes out of business, the device could stop working or need
;; to be replaced, potentially much more quickly than a non-smart version would
;; have. Many have advocated, and in some places passed, so called "Right to
;; repair" laws that ensure consumers have access to the ability to fix or keep
;; running devices: one of the first in the US was actually for cars, where a
;; law in Massachussetts guaranteed owners the same access to repair information
;; as car dealers.
;;
;; FIRST TASK: Please identify which privacy concerns you think are most
;; important for smart home devices. Write no more than 2-3 sentences.
;;
;; SECOND TASK: Please identify which "right to repair" principles are most
;; important for smart home devices. Write no more than 2-3 sentences.

;;!! IMPORTANT: Write your response BELOW this line:

#|

First Task: Privacy Concerns

The most important privacy concern for smart home devices is the transmission of sensitive data,
such as voice recordings or video footage, to servers that could be hacked or misused.
Ensuring transparency about data usage is critical to protecting users from surveillance or
unauthorized access.

Second Task: Right to Repair Principles

The most important "right to repair" principle for smart home devices is ensuring access to repair
documentation and software updates. Additionally, devices should be designed to function locally
without reliance on remote servers, so that the company can function if it loses its serves.

|#


;;! Problem 6
;;
;; Design a function `remove-devices` that, given a list of devices, removes
;; some of them according to the following strategy, and returns the remaining
;; ones:
;;
;; - The first device should be kept.
;; - If the first one has energy-consumption n, then the next m devices should be removed, until
;;   the total energy of the m devices adds to n or greater (or the end of the list is reached).
;; - This should then repeat with the next device being treated like the first one.
;;
;;
;; One test is provided as an example:
(check-expect (remove-devices (list (make-device "A" 100)
                                    (make-device "B" 60)
                                    (make-device "C" 50)
                                    (make-device "D" 30)
                                    (make-device "E" 100)
                                    (make-device "F" 10)))
              (list (make-device "A" 100)
                    (make-device "D" 30)
                    (make-device "F" 10)))

;;!! IMPORTANT: Write your response BELOW this line:

;; remove-devices : [List-of Devices] -> [List-of Devices]
;; given a list of devices, removes some of them according to the following strategy, and returns
;; the remaining ones:
;; - the first device should be kept.
;; - if the first one has energy-consumption n, then the next m devices should be removed, until
;;   the total energy of the m devices adds to n or greater (or the end of the list is reached).
;; - this should then repeat with the next device being treated like the first one.
(define (remove-devices devices)
  (local [
          (define (helper devices sum)
            (cond
              [(or (empty? devices) (<= sum 0)) devices]
              [else
               (helper (rest devices)
                                       (- sum (device-energy-consumption (first devices))))]))
          ]
    (if (empty? devices)
        '()
        (cons (first devices)
              (remove-devices (helper (rest devices)
                                          (device-energy-consumption (first devices))))))))

;; Test Cases
(check-expect (remove-devices (list (make-device "A" 100)
                                    (make-device "B" 60)
                                    (make-device "C" 50)
                                    (make-device "D" 30)
                                    (make-device "E" 100)
                                    (make-device "F" 10)))
              (list (make-device "A" 100)
                    (make-device "D" 30)
                    (make-device "F" 10)))
(check-expect (remove-devices (list (make-device "A" 50)
                                    (make-device "B" 30)
                                    (make-device "C" 20)
                                    (make-device "D" 10)))
              (list (make-device "A" 50)
                    (make-device "D" 10)))
(check-expect (remove-devices (list (make-device "A" 60)
                                    (make-device "B" 40)
                                    (make-device "C" 30)
                                    (make-device "D" 10)))
              (list (make-device "A" 60)
                    (make-device "D" 10)))