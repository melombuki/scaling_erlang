This is some Erlang code written while following along with the book Designing for Scalability with Erlang/OTP.

Compile:
  ```console
  $ erl -make
  ```
Start:
  ```bash
  $ erl -pa ebin
  ```

Run:
  ```erl
  1> application:start(sasl).
  2> application:start(bsc).
  3> bsc:start_test(10, 10).
  ```
