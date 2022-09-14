\documentclass{article}
%include polycode.fmt
\begin{document}
\section*{Laws and properties of the Traversable class}
\textbf{1. Identity}traverse Identity = Identity
\begin{code}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}
module StepikHaskellConspect where

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap g (Identity x) = Identity (g x)
    
instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    (<*>) :: Functor f => Identity (a -> b) -> f a -> f b
    Identity g <*> v = fmap g v
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse Identity == Identity 
instance Traversable Identity where
    traverse Identity = Identity
\end{code}
\begin{semicode}
This law says that traversing the data constructor of the Identity
type over a value will produce the same result as just putting the
value in Identity. This tells us Identity represents a “structural”
identity for traversing data. This is another way of saying that
a Traversable instance cannot add or inject any structure or
“effects.”
\end{semicode}
\end{document}
% lhs2TeX -o Tasks.tex Tasks.lhs
%pdflatex hello.tex