for file in tests/*; do
	./test-parser.sh -f $(basename $file)
done
