const pusher = new Pusher('fd32b87508ffffee4257', { cluster: 'us2' })

exports.subscribe_ = function (channelName, fn) {
  const channel = pusher.subscribe(channelName)
  channel.bind('reacted', (data) => {
    fn(data)()
  })
}
