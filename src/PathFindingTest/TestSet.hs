module PathFindingTest.TestSet(PathingMapTest(..), tests) where

  import PathFindingCore.PathingMap.Interpreter(PathingMapString(..))

  data PathingMapTest = PathingMapTest { dist :: Maybe Double, pathingMapStr :: PathingMapString }

  tests :: [PathingMapTest]
  tests = [testMap1,  testMap2,  testMap3,  testMap4,  testMap5,  testMap6,  testMap7,  testMap8,  testMap9,  testMap10
          ,testMap11, testMap12, testMap13, testMap14, testMap15, testMap16, testMap17, testMap18, testMap19, testMap20
          ,testMap21, testMap22, testMap23, testMap24, testMap25, testMap26, testMap27, testMap28, testMap29, testMap30
          ,testMap31, testMap32, testMap33, testMap34, testMap35, testMap36, testMap37, testMap38, testMap39
          ]

  pms :: String -> String -> PathingMapString
  pms = flip PathingMapString

  testMap1 :: PathingMapTest
  testMap1 = PathingMapTest (Just 14) $ pms "akjshdkjashldjaksdhljakds" "*             G"

  testMap2 :: PathingMapTest
  testMap2 = PathingMapTest (Just 2) $ pms "asdf" " *asdf\
                                                  \G asdf"

  testMap3 :: PathingMapTest
  testMap3 = PathingMapTest Nothing $ pms "|" " %  *|\
                                              \OG% %|\
                                              \%%   |"

  testMap4 :: PathingMapTest
  testMap4 = PathingMapTest (Just 6) $ pms "|" " %  *|\
                                               \OG% %|\
                                               \%    |"

  testMap5 :: PathingMapTest
  testMap5 = PathingMapTest (Just 39) $ pms "|" "               |\
                                                \           *   |\
                                                \               |\
                                                \               |\
                                                \%%%%%%%%%%     |\
                                                \        GD     |\
                                                \D DDDDDDDD     |\
                                                \  D D    D     |\
                                                \ DD      D     |\
                                                \    D DDDD     |\
                                                \DDDDD    D     |\
                                                \    DDDD D     |\
                                                \               |\
                                                \               |\
                                                \               "

  testMap6 :: PathingMapTest
  testMap6 = PathingMapTest (Just 61) $ pms "|" "               |\
                                                \           *   |\
                                                \         O%%%%%|\
                                                \               |\
                                                \%%%%%%%%%%%%%% |\
                                                \        GD     |\
                                                \D DDDDDDDD %%%%|\
                                                \  D D    D     |\
                                                \ DD      D%%%% |\
                                                \    D DDDD     |\
                                                \DDDDD    D     |\
                                                \    DDDD D     |\
                                                \       % %     |\
                                                \       % %     |\
                                                \               "

  testMap7 :: PathingMapTest
  testMap7 = PathingMapTest Nothing $ pms "|" "*DG"

  testMap8 :: PathingMapTest
  testMap8 = PathingMapTest (Just 14) $ pms "|" "G             *"

  testMap9 :: PathingMapTest
  testMap9 = PathingMapTest (Just 14) $ pms "|" "*|\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \G"

  testMap10 :: PathingMapTest
  testMap10 = PathingMapTest (Just 14) $ pms "|" "G|\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \ |\
                                                 \*"

  testMap11 :: PathingMapTest
  testMap11 = PathingMapTest (Just 7) $ pms "|" "       *      G"

  testMap12 :: PathingMapTest
  testMap12 = PathingMapTest (Just 8) $ pms "|" " |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \*|\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \ |\
                                                \G"

  testMap13 :: PathingMapTest
  testMap13 = PathingMapTest (Just 14) $ pms "|" "*             G|\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \               "

  testMap14 :: PathingMapTest
  testMap14 = PathingMapTest (Just 14) $ pms "|" "G             *|\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \               "

  testMap15 :: PathingMapTest
  testMap15 = PathingMapTest (Just 14) $ pms "|" "               |\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \*             G"

  testMap16 :: PathingMapTest
  testMap16 = PathingMapTest (Just 14) $ pms "|" "               |\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \G             *"

  testMap17 :: PathingMapTest
  testMap17 = PathingMapTest (Just 14) $ pms "|" "               |\
                                                 \               |\
                                                 \*             G|\
                                                 \               |\
                                                 \               "

  testMap18 :: PathingMapTest
  testMap18 = PathingMapTest (Just 4) $ pms "|" "*              |\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \G              "

  testMap19 :: PathingMapTest
  testMap19 = PathingMapTest (Just 4) $ pms "|" "G              |\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \*              "

  testMap20 :: PathingMapTest
  testMap20 = PathingMapTest (Just 4) $ pms "|" "              *|\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \              G"

  testMap21 :: PathingMapTest
  testMap21 = PathingMapTest (Just 4) $ pms "|" "              G|\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \              *"

  testMap22 :: PathingMapTest
  testMap22 = PathingMapTest (Just 4) $ pms "|" "       *       |\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \       G       "

  testMap23 :: PathingMapTest
  testMap23 = PathingMapTest (Just 4) $ pms "|" "       G       |\
                                                \               |\
                                                \               |\
                                                \               |\
                                                \       *       "

  testMap24 :: PathingMapTest
  testMap24 = PathingMapTest (Just 18) $ pms "|" "              G|\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \*              "

  testMap25 :: PathingMapTest
  testMap25 = PathingMapTest (Just 18) $ pms "|" "G              |\
                                                 \               |\
                                                 \               |\
                                                 \               |\
                                                 \              *"

  testMap26 :: PathingMapTest
  testMap26 = PathingMapTest (Just 9) $ pms "|" "G              |\
                                                \               |\
                                                \       *       |\
                                                \               |\
                                                \               "

  testMap27 :: PathingMapTest
  testMap27 = PathingMapTest (Just 20) $ pms "|" "GD DD   D      |\
                                                 \   DD  D  D D  |\
                                                 \ D      D      |\
                                                 \    D  D     D |\
                                                 \ D  D      D  *"

  testMap28 :: PathingMapTest
  testMap28 = PathingMapTest (Just 4) $ pms "|" "              G|\
                                                \             D |\
                                                \             D |\
                                                \             D |\
                                                \             D*"

  testMap29 :: PathingMapTest
  testMap29 = PathingMapTest (Just 32) $ pms "|" "G              |\
                                                 \               |\
                                                 \               |\
                                                 \DDDDDDDDDDDDDD |\
                                                 \*              "

  testMap30 :: PathingMapTest
  testMap30 = PathingMapTest (Just 15) $ pms "|" "      D        |\
                                                 \      D        |\
                                                 \      D*D      |\
                                                 \      DDD      |\
                                                 \G              "

  testMap31 :: PathingMapTest
  testMap31 = PathingMapTest (Just 13) $ pms "|" "               |\
                                                 \      D D      |\
                                                 \      D*D      |\
                                                 \      DDD      |\
                                                 \G              "

  testMap32 :: PathingMapTest
  testMap32 = PathingMapTest (Just 13) $ pms "|" "        D      |\
                                                 \      D D      |\
                                                 \      D*D      |\
                                                 \      DDD      |\
                                                 \G              "

  testMap33 :: PathingMapTest
  testMap33 = PathingMapTest (Just 9) $ pms "|" "      D        |\
                                                \      D        |\
                                                \      D*D      |\
                                                \      D D      |\
                                                \G              "

  testMap34 :: PathingMapTest
  testMap34 = PathingMapTest (Just 9) $ pms "|" "      D        |\
                                                \      D        |\
                                                \       *D      |\
                                                \      DDD      |\
                                                \G              "

  testMap35 :: PathingMapTest
  testMap35 = PathingMapTest Nothing $ pms "|" "               |\
                                               \      DDD      |\
                                               \      D*D      |\
                                               \      DDD      |\
                                               \G              "

  testMap36 :: PathingMapTest
  testMap36 = PathingMapTest Nothing $ pms "|" "                                              |\
                                               \                                              |\
                                               \                                              |\
                                               \                 DDDDDDDDDDDDD                |\
                                               \                 D    D  D   D                |\
                                               \                 D  D        D                |\
                                               \                 D         D D                |\
                                               \                 D D         D                |\
                                               \                 D    *      D                |\
                                               \                 D          DD                |\
                                               \                 D   D       D                |\
                                               \                 DD  D   D   D                |\
                                               \                 DDDDDDDDDDDDD                |\
                                               \                                              |\
                                               \       G                                      |\
                                               \                                              |\
                                               \                                              "

  testMap37 :: PathingMapTest
  testMap37 = PathingMapTest Nothing $ pms "|" "               |\
                                               \      DDD      |\
                                               \      DGD      |\
                                               \      DDD      |\
                                               \*              "

  testMap38 :: PathingMapTest
  testMap38 = PathingMapTest Nothing $ pms "|" "                                              |\
                                               \                                              |\
                                               \                                              |\
                                               \                 DDDDDDDDDDDDD                |\
                                               \                 D    D  D   D                |\
                                               \                 D  D        D                |\
                                               \                 D         D D                |\
                                               \                 D D         D                |\
                                               \                 D    G      D                |\
                                               \                 D          DD                |\
                                               \                 D   D       D                |\
                                               \                 DD  D   D   D                |\
                                               \                 DDDDDDDDDDDDD                |\
                                               \                                              |\
                                               \       *                                      |\
                                               \                                              |\
                                               \                                              "

  testMap39 :: PathingMapTest
  testMap39 = PathingMapTest Nothing $ pms "|" "                                              |\
                                               \ *                                            |\
                                               \                DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|\
                                               \                DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|\
                                               \                DD    D  D                    |\
                                               \                DD  D    f   DDDDDDDDDDDDDDDD |\
                                               \                DD       f D DD             D |\
                                               \                DD D      fffDD            D  |\
                                               \                DD    G      DD             D |\
                                               \                DD   D      DDD DDDDDDDDDDDDD |\
                                               \                DD   D   D   DD DD            |\
                                               \ DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD DDDDDDDDDDDDD |\
                                               \ DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD D D   D   D |\
                                               \                          D   D   D D D D D D |\
                                               \DDDDDDDDDDDDDDDDDDDDDDDDD   D     D   D   D   |\
                                               \DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD|\
                                               \                        DDDDDDDDDDDDDDDDDDDDDD"
