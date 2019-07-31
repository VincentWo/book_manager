command="../main view --data=1.json"
echo "Testing: $command"

RESULT=$($command)
EXPECTED_RESULT=$(echo)

if [[ $RESULT = $EXPECTED_RESULT ]]; then
		echo "Test passed"
		exit 0
else
		echo "Assertion failed:" 
		echo "Expected Output: " 
		echo $EXPECTED_RESULT    
		echo                     
		echo "Real Output: "     
		echo "$RESULT"           
		exit 1
fi
