.PHONY: all clean jenga
jenga:
	jenga -P -j 4 -act -progress

clean:
	git clean -fdx
