\documentclass{article}
%include polycode.fmt
\begin{document}
\section*{Laws and properties of the Traversable class}
\textbf{перевод:}Законы и свойства класса Traversable
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

\end{code}
\end{document}
% lhs2TeX -o Tasks.tex Tasks.lhs
%pdflatex hello.tex