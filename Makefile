all : haskinator

haskinator : Haskinator.hs
	ghc --make -o haskinator -main-is Haskinator.main Haskinator.hs
clean :
	rm *.o *.hi haskinator




