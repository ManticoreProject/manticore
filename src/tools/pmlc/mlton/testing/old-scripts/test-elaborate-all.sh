for file in elaborate-tests/*; do
	./test-elaborate.sh -f $(basename $file)
done
