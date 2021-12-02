# Ruby Tips & Tricks


This file captures my notes about developing solutions using [Ruby][rubylang].


**Contents**

- [Making code REPL-friendly](#making-code-repl-friendly)
- [Working in the REPL](#working-in-the-repl)
- [Looking up docs](#looking-up-docs)





<a id="making-code-repl-friendly"></a>
## Making code REPL-friendly


It's nice to be able to load the code in the IRB REPL
and to be able to run it from the command line.
I do that by adding this block of code
to the top level of my solution files:

```ruby
if $PROGRAM_NAME == __FILE__
  # Run the program
end
```

The code inside that block will run when I run from the command line,
but not when loading it in IRB.





<a id="working-in-the-repl"></a>
## Working in the REPL


Ruby's IRB is a pretty good REPL.
It supports hot code reloading,
even updating existing objects when you change their class.
Here's how I use IRB during development.

I open a long-lived IRB session in my project directory
in a terminal.
Then I use the `load` function to load my source code,
and to update it every time I make a change.





<a id="looking-up-docs"></a>
## Looking up docs


```
irb(main)> load './solution.rb'
```

The `help` command is convenient
for getting docs about methods.

```
irb(main)> help
>> each_cons
>> collect
>> Enumerable#collect
>> Enumerable
```





<!-- References -->

[rubylang]: https://www.ruby-lang.org/en/
