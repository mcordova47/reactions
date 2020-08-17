require 'pusher'

class PublishController < ApplicationController
  PUSHER = Pusher::Client.new(
    app_id: '1056078',
    key: 'd9473141e15ccf0c9d86',
    secret: '14e5dff2a2e71c99f0b9',
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
