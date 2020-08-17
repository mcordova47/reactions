const pusher = new Pusher('d9473141e15ccf0c9d86', { cluster: 'us2' })

exports.subscribe_ = function (channelName, fn) {
  const channel = pusher.subscribe(channelName)
  channel.bind('reacted', (data) => {
    fn(data)()
  })
}
