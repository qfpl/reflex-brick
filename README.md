Playing around with some ideas for integrating `reflex` and `brick` without having to expose the internals of `brick`.

At the moment I'm using `reflex-basic-host`, but I think I'm going to end up writing a custom host for this in order to be coordinate between `brick` and any `IO` performed via the `PerformEvent` instances.
