defmodule Account do
  defstruct balance: 0, status: :open
end

defmodule BankAccount do
  use GenServer

  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  #########
  ## API ##
  #########

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, pid} = GenServer.start_link(__MODULE__, :no_args)
    pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    GenServer.call(account, :close_account)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    GenServer.call(account, :balance)
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    GenServer.call(account, {:update, amount})
  end

  ###########################
  ## GenServer Implementation
  ###########################

  # init: Accounts start with 0 balance and status open
  def init(_) do
    account = %Account{balance: 0, status: :open}
    {:ok, account}
  end

  # No calls work when account has status == :closed
  def handle_call(_any, _from, %Account{status: status} = account) when status == :closed do
    msg = {:error, :account_closed}
    {:reply, msg, account}
  end

  # :balance -> Returns balance with no side effects
  def handle_call(:balance, _from, %Account{balance: balance} = account) do
    {:reply, balance, account}
  end

  # {:update, balance_delta} -> Returns :ok with side effect of adding balance_delta to balance
  def handle_call({:update, balance_delta}, _from, %Account{balance: balance} = account) do
    new_account = %Account{account | balance: balance_delta + balance}
    {:reply, :ok, new_account}
  end

  # :close_account -> Returns :ok with side effect of setting status to :closed
  def handle_call(:close_account, _from, account) do
    new_account = %Account{account | status: :closed}
    {:reply, :ok, new_account}
  end
end
