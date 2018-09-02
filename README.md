# lambda-calculus

## input

	zero = (\x -> (\y -> y))                                                                            
	one = (succ zero)                                                                                   
	two = (succ one)                                                                                    
	three = (succ two)                                                                                  
	four = (succ three)                                                                                 
	five = (succ four)                                                                                  
	six = (succ five)                                                                                   
	seven = (succ six)                                                                                  
	eight = (succ seven)                                                                                
	nine = (succ eight)                                                                                 
	ten = (succ nine)                                                                                   
	succ = (\n -> (\s -> (\z -> (s ((n s) z)))))                                                        
	add = (\n -> (\m -> (\s -> (\z -> ((n s) ((m s) z))))))                                             
	mult = (\n -> (\m -> (\s -> (\z -> ((n (m s)) z)))))                                                
	power = (\n -> (\m -> (\s -> (\z -> (((m n) s) z)))))                                               
	main = ((mult two) seven)    

## output

	main
	((mult two) seven)
	(((\n -> (\m -> (\s -> (\z -> ((n (m s)) z))))) two) seven)
	((\m -> (\s -> (\z -> ((two (m s)) z)))) seven)
	(\s -> (\z -> ((two (seven s)) z)))
	(\s -> (\z -> (((succ one) (seven s)) z)))
	(\s -> (\z -> ((((\n -> (\s -> (\z -> (s ((n s) z))))) one) (seven s)) z)))
	(\s -> (\z -> (((\s -> (\z -> (s ((one s) z)))) (seven s)) z)))
	(\s -> (\z -> ((\z -> ((seven s) ((one (seven s)) z))) z)))
	(\s -> (\z -> ((seven s) ((one (seven s)) z))))
	(\s -> (\z -> (((succ six) s) ((one (seven s)) z))))
	(\s -> (\z -> ((((\n -> (\s -> (\z -> (s ((n s) z))))) six) s) ((one (seven s)) z))))
	(\s -> (\z -> (((\s -> (\z -> (s ((six s) z)))) s) ((one (seven s)) z))))
	(\s -> (\z -> ((\z -> (s ((six s) z))) ((one (seven s)) z))))
	(\s -> (\z -> (s ((six s) ((one (seven s)) z)))))
	(\s -> (\z -> (s (((succ five) s) ((one (seven s)) z)))))
	(\s -> (\z -> (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) five) s) ((one (seven s)) z)))))
	(\s -> (\z -> (s (((\s -> (\z -> (s ((five s) z)))) s) ((one (seven s)) z)))))
	(\s -> (\z -> (s ((\z -> (s ((five s) z))) ((one (seven s)) z)))))
	(\s -> (\z -> (s (s ((five s) ((one (seven s)) z))))))
	(\s -> (\z -> (s (s (((succ four) s) ((one (seven s)) z))))))
	(\s -> (\z -> (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) four) s) ((one (seven s)) z))))))
	(\s -> (\z -> (s (s (((\s -> (\z -> (s ((four s) z)))) s) ((one (seven s)) z))))))
	(\s -> (\z -> (s (s ((\z -> (s ((four s) z))) ((one (seven s)) z))))))
	(\s -> (\z -> (s (s (s ((four s) ((one (seven s)) z)))))))
	(\s -> (\z -> (s (s (s (((succ three) s) ((one (seven s)) z)))))))
	(\s -> (\z -> (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) three) s) ((one (seven s)) z)))))))
	(\s -> (\z -> (s (s (s (((\s -> (\z -> (s ((three s) z)))) s) ((one (seven s)) z)))))))
	(\s -> (\z -> (s (s (s ((\z -> (s ((three s) z))) ((one (seven s)) z)))))))
	(\s -> (\z -> (s (s (s (s ((three s) ((one (seven s)) z))))))))
	(\s -> (\z -> (s (s (s (s (((succ two) s) ((one (seven s)) z))))))))
	(\s -> (\z -> (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) two) s) ((one (seven s)) z))))))))
	(\s -> (\z -> (s (s (s (s (((\s -> (\z -> (s ((two s) z)))) s) ((one (seven s)) z))))))))
	(\s -> (\z -> (s (s (s (s ((\z -> (s ((two s) z))) ((one (seven s)) z))))))))
	(\s -> (\z -> (s (s (s (s (s ((two s) ((one (seven s)) z)))))))))
	(\s -> (\z -> (s (s (s (s (s (((succ one) s) ((one (seven s)) z)))))))))
	(\s -> (\z -> (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) one) s) ((one (seven s)) z)))))))))
	(\s -> (\z -> (s (s (s (s (s (((\s -> (\z -> (s ((one s) z)))) s) ((one (seven s)) z)))))))))
	(\s -> (\z -> (s (s (s (s (s ((\z -> (s ((one s) z))) ((one (seven s)) z)))))))))
	(\s -> (\z -> (s (s (s (s (s (s ((one s) ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (((succ zero) s) ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) zero) s) ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (((\s -> (\z -> (s ((zero s) z)))) s) ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s ((\z -> (s ((zero s) z))) ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((zero s) ((one (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (((\x -> (\y -> y)) s) ((one (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((\y -> y) ((one (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((one (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (((succ zero) (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) zero) (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (((\s -> (\z -> (s ((zero s) z)))) (seven s)) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((\z -> ((seven s) ((zero (seven s)) z))) z))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((seven s) ((zero (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (((succ six) s) ((zero (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) six) s) ((zero (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (((\s -> (\z -> (s ((six s) z)))) s) ((zero (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s ((\z -> (s ((six s) z))) ((zero (seven s)) z)))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s ((six s) ((zero (seven s)) z))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (((succ five) s) ((zero (seven s)) z))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) five) s) ((zero (seven s)) z))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((five s) z)))) s) ((zero (seven s)) z))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s ((\z -> (s ((five s) z))) ((zero (seven s)) z))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s ((five s) ((zero (seven s)) z)))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (((succ four) s) ((zero (seven s)) z)))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) four) s) ((zero (seven s)) z)))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((four s) z)))) s) ((zero (seven s)) z)))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s ((\z -> (s ((four s) z))) ((zero (seven s)) z)))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s ((four s) ((zero (seven s)) z))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (((succ three) s) ((zero (seven s)) z))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) three) s) ((zero (seven s)) z))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((three s) z)))) s) ((zero (seven s)) z))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s ((\z -> (s ((three s) z))) ((zero (seven s)) z))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s ((three s) ((zero (seven s)) z)))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (((succ two) s) ((zero (seven s)) z)))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) two) s) ((zero (seven s)) z)))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((two s) z)))) s) ((zero (seven s)) z)))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s ((\z -> (s ((two s) z))) ((zero (seven s)) z)))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s ((two s) ((zero (seven s)) z))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (((succ one) s) ((zero (seven s)) z))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) one) s) ((zero (seven s)) z))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((one s) z)))) s) ((zero (seven s)) z))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s ((\z -> (s ((one s) z))) ((zero (seven s)) z))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s ((one s) ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (((succ zero) s) ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s ((((\n -> (\s -> (\z -> (s ((n s) z))))) zero) s) ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (((\s -> (\z -> (s ((zero s) z)))) s) ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s ((\z -> (s ((zero s) z))) ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s ((zero s) ((zero (seven s)) z))))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s (((\x -> (\y -> y)) s) ((zero (seven s)) z))))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s ((\y -> y) ((zero (seven s)) z))))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s ((zero (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s (((\x -> (\y -> y)) (seven s)) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s ((\y -> y) z)))))))))))))))))
	(\s -> (\z -> (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))

