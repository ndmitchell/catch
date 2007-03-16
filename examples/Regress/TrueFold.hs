-- #CATCH {: {True} * : {True},[]} | {[]}

module TrueFold where

main xs = foldl trues True xs

trues True True = True
