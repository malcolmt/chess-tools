{-
 - Runs all the tests and provides a nice summary output, using the
 - test-framework library.
 -
 - Useful during development. During deployment situations, "cabal test" is
 - going to be more appropriate.
 -}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import TestBoard


main = defaultMain tests

tests = [
        testGroup "Board arrays" [
            testProperty "index to square" prop_index_to_square_inverse,
            testProperty "square to index" prop_square_to_index_inverse,
            testProperty "indices increase" prop_index_increases_with_square,
            testProperty "array size" prop_board_array_size
            ],
        testGroup "Lookup arrays" [
            testProperty "repIndexList represents"
                    prop_repIndexList_is_representative,
            testProperty "file lookup 1" prop_check_file_distance_1,
            testProperty "file lookup 2" prop_check_file_distance_2,
            testProperty "rank lookup 1" prop_check_rank_distance_1,
            testProperty "rank lookup 2" prop_check_rank_distance_2,
            testProperty "square lookup 1" prop_check_square_distance_1,
            testProperty "square lookup 2" prop_check_square_distance_2
            ]
        ]

