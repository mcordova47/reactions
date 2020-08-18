require 'pusher'

class PublishController < ApplicationController
  PUSHER = Pusher::Client.new(
    app_id: '1056078',
    key: 'fd32b87508ffffee4257',
    secret: ENV["PUSHER_SECRET"],
    cluster: 'us2'
  )

  skip_before_action :verify_authenticity_token

  def publish
    PUSHER.trigger params[:channel], 'reacted', reaction: params[:reaction]
    respond_to do |format|
      format.json { render json: nil }
    end
  end
end
