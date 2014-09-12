# This script runs testing with cabal and collects the output to 'test_output' file.
# Then in greps expected and actual results to 'expected' and 'got' files respectively.
# Then it launches a specified diff command (for example opendiff).
# Finally it deletes all three created files.
cabal test > test_output
grep expected test_output > expected
grep got test_output > got
$1 expected got
rm test_output expected got
