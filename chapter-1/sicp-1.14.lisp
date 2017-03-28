(in-package :sicp)



;; *Exercise 1.14:* Draw the tree illustrating the process generated
;; by the `count-change' procedure of section *Note 1-2-2:: in making
;; change for 11 cents.  What are the orders of growth of the space
;; and number of steps used by this process as the amount to be
;; changed increases?


(trace cc)

(count-change 11)

#|
  0: (CC 11 5)
    1: (CC 11 4)
      2: (CC 11 3)
        3: (CC 11 2)
          4: (CC 11 1)
            5: (CC 11 0)
            5: CC returned 0
            5: (CC 10 1)
              6: (CC 10 0)
              6: CC returned 0
              6: (CC 9 1)
                7: (CC 9 0)
                7: CC returned 0
                7: (CC 8 1)
                  8: (CC 8 0)
                  8: CC returned 0
                  8: (CC 7 1)
                    9: (CC 7 0)
                    9: CC returned 0
                    9: (CC 6 1)
                      10: (CC 6 0)
                      10: CC returned 0
                      10: (CC 5 1)
                        11: (CC 5 0)
                        11: CC returned 0
                        11: (CC 4 1)
                          12: (CC 4 0)
                          12: CC returned 0
                          12: (CC 3 1)
                            13: (CC 3 0)
                            13: CC returned 0
                            13: (CC 2 1)
                              14: (CC 2 0)
                              14: CC returned 0
                              14: (CC 1 1)
                                15: (CC 1 0)
                                15: CC returned 0
                                15: (CC 0 1)
                                15: CC returned 1
                              14: CC returned 1
                            13: CC returned 1
                          12: CC returned 1
                        11: CC returned 1
                      10: CC returned 1
                    9: CC returned 1
                  8: CC returned 1
                7: CC returned 1
              6: CC returned 1
            5: CC returned 1
          4: CC returned 1
          4: (CC 6 2)
            5: (CC 6 1)
              6: (CC 6 0)
              6: CC returned 0
              6: (CC 5 1)
                7: (CC 5 0)
                7: CC returned 0
                7: (CC 4 1)
                  8: (CC 4 0)
                  8: CC returned 0
                  8: (CC 3 1)
                    9: (CC 3 0)
                    9: CC returned 0
                    9: (CC 2 1)
                      10: (CC 2 0)
                      10: CC returned 0
                      10: (CC 1 1)
                        11: (CC 1 0)
                        11: CC returned 0
                        11: (CC 0 1)
                        11: CC returned 1
                      10: CC returned 1
                    9: CC returned 1
                  8: CC returned 1
                7: CC returned 1
              6: CC returned 1
            5: CC returned 1
            5: (CC 1 2)
              6: (CC 1 1)
                7: (CC 1 0)
                7: CC returned 0
                7: (CC 0 1)
                7: CC returned 1
              6: CC returned 1
              6: (CC -4 2)
              6: CC returned 0
            5: CC returned 1
          4: CC returned 2
        3: CC returned 3
        3: (CC 1 3)
          4: (CC 1 2)
            5: (CC 1 1)
              6: (CC 1 0)
              6: CC returned 0
              6: (CC 0 1)
              6: CC returned 1
            5: CC returned 1
            5: (CC -4 2)
            5: CC returned 0
          4: CC returned 1
          4: (CC -9 3)
          4: CC returned 0
        3: CC returned 1
      2: CC returned 4
      2: (CC -14 4)
      2: CC returned 0
    1: CC returned 4
    1: (CC -39 5)
    1: CC returned 0
0: CC returned 4

|#
