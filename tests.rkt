#lang racket
(require rackunit
  rackunit/text-ui
         "scanner.rkt"
         "main.rkt")

;; Define the test input strings to validate the scanner and main file
(define test-inputs
  (list
   (list 'valid-input1 "x := 10; PRINT x"
         '((ID "x") (assign-op ":=") (Integer "10") (symbol ";")
           (keyword "PRINT") (ID "x")))
   (list 'invalid-input1 "@invalidToken; y := 15" 'exception)
   (list 'valid-input2 "a := 5; b := a + 7; PRINT b"
         '((ID "a") (assign-op ":=") (Integer "5") (symbol ";")
           (ID "b") (assign-op ":=") (ID "a") (operator "+") (Integer "7")
           (symbol ";") (keyword "PRINT") (ID "b")))
   (list 'valid-input3 "x := y + 23 * (sqrt a) :"
         '((ID "x") (assign-op ":=") (ID "y") (add-op "+") (Integer "23")
           (Mult-Exp "*") (symbol "(") (ID "sqrt") (ID "a") (symbol ")") (colon ":")))))

;; Helper function to run scanner tests
(define (run-scanner-test input-name input expected)
  (test-case (format "Scanner Test - ~a" input-name)
    (lambda ()
      (let ([result (scan-input (list input))])
        ;; Debug output outside the actual checks for better reliability
        (displayln (format "Running scanner test for ~a" input-name))
        (displayln (format "Expected Tokens: ~a" expected))
        (displayln (format "Generated Tokens: ~a" result))
        ;; Add detailed comparison to ensure every token matches what is expected
        (for-each
         (lambda (expected-token actual-token)
           (unless (equal? expected-token actual-token)
             (error (format "Mismatch in tokens for ~a: Expected ~a but got ~a"
                            input-name expected-token actual-token))))
         expected result)
        ;; Overall equality check
        (check-equal? result expected
                      (format "Testing scanner with ~a failed. Expected: ~a, Got: ~a"
                              expected result))))))

;; Helper function to run main file workflow tests
(define (run-main-workflow-test input-name input)
  (test-case (format "Main Workflow w/ ~a" input-name)
    (lambda ()
      (let* ([tokens (scan-input (list input))]
             [result (process tokens)])
        ;; Debug output outside the actual checks for better reliability
        (displayln (format "Running main workflow test for ~a" input-name))
        (displayln (format "Tokens: ~a" tokens))
        (displayln (format "Result: ~a" result))
        (unless (equal? result 'success)
          (error (format "Main workflow for ~a failed with result: ~a"
                         input-name result)))
        (check-equal? result 'success
                      (format "Testing main workflow with ~a failed. Got result: ~a"
                              input-name result))))))

;; Create test suites
(define scanner-tests
  (test-suite "Scanner Evaluation"
    (for-each (lambda (test-input)
                (apply run-scanner-test test-input))
              test-inputs)))

(define main-workflow-tests
  (test-suite "Main File Evaluation"
    (for-each (lambda (test-input)
                (when (not (eq? (third test-input) 'exception))
                  (run-main-workflow-test (first test-input) (second test-input))))
              test-inputs)))

;; Combine both test suites into one
(define full-test-suite
  (test-suite "Full System Test Suite"
    scanner-tests
    main-workflow-tests))

;; Run all tests using text-ui to ensure visibility of output
(run-tests full-test-suite 'verbose)