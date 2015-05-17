Provides a Lens API to send notifications via the `notify-send` command

```haskell
notifySend $ defaultNotification & body .~ "hello world"
```
