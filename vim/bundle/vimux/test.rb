class TmuxSession
  def initialize(session, window, pane)
    @session = session
    @window = window
    @pane = pane
    @runner_pane = vim_cached_runner_pane
  end

  def vim_cached_runner_pane
    if Vim.evaluate('exists("g:_VimTmuxRunnerPane")') != 0
      Vim.evaluate('g:_VimTmuxRunnerPane')
    else
      nil
    end
  end

  def vim_cached_runner_pane=(runner_pane)
    Vim.command("let g:_VimTmuxRunnerPane = '#{runner_pane}'")
  end

  def height
    if Vim.evaluate('exists("g:VimuxHeight")') != 0
      Vim/evaluate('g:VimuxHeight')
    else
      20
    end
  end

  def current_panes
    run('list-panes').split("\n").map do |line|
      line.split(':').first
    end
  end

  def active_pane_id
    run('list-panes').split("\n").map do |line|
      return line.split[-2] if line =~ /\(active\)/
    end
  end

  def target(args={})
    "#{args.fetch(:session, @session)}:#{args.fetch(:window, @window)}.#{args.fetch(:pane, @pane)}"
  end

  def runner_pane
    if @runner_pane.nil?
      run("split-window -p #{height}")
      @runner_pane = active_pane_id
      vim_cached_runner_pane = @runner_pane
    end

    run('list-panes').split("\n").map do |line|
      return line.split(':').first if line =~ /#{@runner_pane}/
    end
  end

  def run_shell_command(command)
    send_command(command, target(:pane => runner_pane))
    move_up_pane
  end

  def close_other_panes
    run("kill-pane -a")
  end

  def move_up_pane
    run("select-pane -t #{target}")
  end

  def send_command(command, target)
    run("send-keys -t #{target} '#{command.gsub("'", "\'")}'")
    run("send-keys -t #{target} Enter")
  end

  def run(command)
    `tmux #{command}`
  end
end

class CurrentTmuxSession < TmuxSession
  def initialize
    session = self.get_property(:attached, :session)
    window = self.get_property(:active, :window)
    pane = self.get_property(:active, :pane)

    super(session, window, pane)
  end

  def get_property(match, type)
    run("list-#{type.to_s}").split("\n").each do |line|
      return line.split(':').first if line =~ /\(#{match.to_s}\)/
    end
  end
end
