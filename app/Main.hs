module Main (main) where

import Lambda
import Parser
import System.Console.Haskeline

main :: IO ()
main = do
    -- putStr "\n"
    -- runp "@plus(1)(2)"
    -- putStr "\n"
    -- runp "@plus(2)(1)"
    -- putStr "\n"
    -- runp "@plus(3)(0)"
    -- putStr "\n"
    -- runp "@plus(0)(3)"
    -- putStr "\n"

    let t1 = myparse "\\$.\\#.$((\\#.$($#))#)"
    -- let t2 = myparse "\\$.\\#.$($((\\#.$#)#))"
    -- let t3 = myparse "\\$.\\#.$($($((\\#.#)#)))"
    -- let t4 = myparse "\\$.\\#.(\\#.$($($#)))#"
    -- print t1
    print (finalTerm (reduce t1))
    -- print (finalTerm (reduce t2))
    -- print (finalTerm (reduce t3))
    -- print (finalTerm (reduce t4))

    print (parseChurch (finalTerm (reduce t1)))
    -- print (parseChurch (finalTerm (reduce t2)))
    -- print (parseChurch (finalTerm (reduce t3)))
    -- print (parseChurch (finalTerm (reduce t4)))
    
    -- print (prettyprint t1)

    -- putStr "\n"
    let t5 = myparse "\\$.\\#.(\\a.\\b.ab)$((\\a.\\b.a(ab))$#)"
    print (finalTerm (reduce t5))
    print (parseChurch (finalTerm (reduce t5)))