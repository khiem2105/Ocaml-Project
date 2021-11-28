run:
	dune exec bin/main.exe
	python3 bin/plot_execute_time.py

clean:
	dune clean
	rm figure/*