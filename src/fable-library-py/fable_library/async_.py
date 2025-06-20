from __future__ import annotations

import asyncio
from asyncio import Future, ensure_future
from collections.abc import Awaitable, Callable, Iterable
from concurrent.futures import ThreadPoolExecutor
from threading import Timer
from typing import (
    Any,
    Literal,
    TypeVar,
)

from .async_builder import (
    Async,
    CancellationToken,
    Continuations,
    IAsyncContext,
    OperationCanceledError,
    Trampoline,
    empty_continuation,
    protected_bind,
    protected_cont,
    protected_return,
    singleton,
)

# F# generated code (from Choice.fs)
from .choice import (
    Choice_makeChoice1Of2,
    Choice_makeChoice2Of2,
    FSharpChoice_2,
)
from .task import TaskCompletionSource
from .time_span import TimeSpan, to_milliseconds


_T = TypeVar("_T")
_U = TypeVar("_U")


def cancellation_token() -> Async[CancellationToken]:
    def cont(ctx: IAsyncContext[Any]):
        ctx.on_success(ctx.cancel_token)

    return protected_cont(cont)


default_cancellation_token = CancellationToken()


# see AsyncBuilder.Delay
def delay(generator: Callable[[], Async[_T]]):
    def cont(ctx: IAsyncContext[_T]):
        generator()(ctx)

    return protected_cont(cont)


def create_cancellation_token(arg: int | bool | None = None) -> CancellationToken:
    cancelled = arg if isinstance(arg, bool) else False
    token = CancellationToken(cancelled)
    if isinstance(arg, int):
        timer = Timer(arg / 1000.0, token.cancel)
        timer.start()

    return token


def cancel(token: CancellationToken) -> None:
    token.cancel()


def cancel_after(token: CancellationToken, ms: int | TimeSpan) -> None:
    ms_value = to_milliseconds(ms)
    timer = Timer(ms_value / 1000.0, token.cancel)
    timer.start()


def is_cancellation_requested(token: CancellationToken) -> bool:
    return token.is_cancelled


def sleep(milliseconds_duetime: int | TimeSpan) -> Async[None]:
    def cont(ctx: IAsyncContext[None]):
        def cancel():
            ctx.on_cancel(OperationCanceledError())

        token_id = ctx.cancel_token.add_listener(cancel)

        def timeout():
            ctx.cancel_token.remove_listener(token_id)
            ctx.on_success(None)

        due_time = to_milliseconds(milliseconds_duetime) / 1000.0
        ctx.trampoline.run_later(timeout, due_time)

    return protected_cont(cont)


def ignore(computation: Async[Any]) -> Async[None]:
    def binder(_: Any | None = None) -> Async[None]:
        return protected_return(None)

    return protected_bind(computation, binder)


def parallel(computations: Iterable[Async[_T]]) -> Async[list[_T]]:
    def delayed() -> Async[list[_T]]:
        tasks: Iterable[Future[_T]] = map(start_as_task, computations)  # type: ignore
        all: Future[list[_T]] = asyncio.gather(*tasks)
        return await_task(all)

    return delay(delayed)


def parallel2(a: Async[_T], b: Async[_U]) -> Async[list[_T | _U]]:
    def delayed() -> Async[list[_T | _U]]:
        tasks: Iterable[Future[_T | _U]] = map(start_as_task, [a, b])  # type: ignore
        all: Future[list[_T | _U]] = asyncio.gather(*tasks)
        return await_task(all)

    return delay(delayed)


def sequential(computations: Iterable[Async[_T]]) -> Async[list[_T]]:
    def delayed() -> Async[list[_T]]:
        results: list[_T] = []

        def _arrow20(_arg: Async[_T]) -> Async[None]:
            cmp: Async[_T] = _arg

            def _arrow19(_arg_1: _T) -> Async[None]:
                result: _T = _arg_1
                (results.append(result))
                return singleton.Zero()

            return singleton.Bind(cmp, _arrow19)

        def _arrow21(__unit: Literal[None] = None) -> Async[list[_T]]:
            return singleton.Return(results)

        return singleton.Combine(singleton.For(computations, _arrow20), singleton.Delay(_arrow21))

    return delay(delayed)


def catch_async(work: Async[_T]) -> Async[FSharpChoice_2[_T, Exception]]:
    def cont(ctx: IAsyncContext[FSharpChoice_2[_T, Exception]]) -> None:
        def on_success(x: _T):
            ctx.on_success(Choice_makeChoice1Of2(x))

        def on_error(err: Exception):
            ctx.on_success(Choice_makeChoice2Of2(err))

        ctx_ = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, on_success, on_error, ctx.on_cancel)
        work(ctx_)

    return protected_cont(cont)


def from_continuations(
    f: Callable[
        [Continuations[_T]],
        None,
    ],
) -> Callable[[IAsyncContext[_T]], None]:
    def cont(ctx: IAsyncContext[_T]) -> None:
        f((ctx.on_success, ctx.on_error, ctx.on_cancel))

    return protected_cont(cont)


def await_task(task: Awaitable[_T]) -> Async[_T]:
    """Return an asynchronous computation that will wait for the given
    task to complete and return its result.
    """
    continuation: Continuations[_T] = (
        empty_continuation,
        empty_continuation,
        empty_continuation,
    )
    task = ensure_future(task)

    def done(tsk: Future[_T]) -> None:
        try:
            value = tsk.result()
        except Exception as ex:
            continuation[1](ex)
        else:
            continuation[0](value)

    def callback(conts: Continuations[_T]) -> None:
        nonlocal continuation
        continuation = conts

    task.add_done_callback(done)
    return from_continuations(callback)


def run_in_loop(computation: Callable[..., None]) -> Any:
    """Run a computation on the event loop.

    If no event loop is running, then one is created and run.
    """

    async def runner() -> None:
        return computation()

    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(runner())
    else:
        return computation()


def start_with_continuations(
    computation: Async[_T],
    continuation: Callable[[_T], None] | None = None,
    exception_continuation: Callable[[Exception], None] | None = None,
    cancellation_continuation: Callable[[OperationCanceledError], None] | None = None,
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Runs an asynchronous computation.

    Runs an asynchronous computation, starting immediately on the
    current operating system thread. Call one of the three continuations
    when the operation completes.

    If no cancellation token is provided then the default cancellation
    token is used."""
    trampoline = Trampoline()

    ctx = IAsyncContext.create(
        trampoline,
        cancellation_token or default_cancellation_token,
        continuation or empty_continuation,
        exception_continuation or empty_continuation,
        cancellation_continuation or empty_continuation,
    )

    def runner() -> None:
        computation(ctx)

    run_in_loop(runner)


def start_as_task(computation: Async[_T], cancellation_token: CancellationToken | None = None) -> Awaitable[_T]:
    """Executes a computation in the thread pool.

    If no cancellation token is provided then the default cancellation
    token is used.
    """
    tcs: TaskCompletionSource[_T] = TaskCompletionSource()

    def resolve(value: _T) -> None:
        tcs.SetResult(value)

    def reject(error: Exception) -> None:
        tcs.SetException(error)

    def cancel(_: OperationCanceledError) -> None:
        tcs.SetCancelled()

    start_with_continuations(
        computation,
        resolve,
        reject,
        cancel,
        cancellation_token or default_cancellation_token,
    )
    return tcs.get_task()


def throw_after(milliseconds_due_time: int | TimeSpan) -> Async[None]:
    def cont(ctx: IAsyncContext[None]) -> None:
        def cancel() -> None:
            ctx.on_cancel(OperationCanceledError())

        token_id = ctx.cancel_token.add_listener(cancel)

        def timeout() -> None:
            ctx.cancel_token.remove_listener(token_id)
            ctx.on_error(TimeoutError())

        due_time_ms = to_milliseconds(milliseconds_due_time)
        ctx.trampoline.run_later(timeout, due_time_ms / 1000.0)

    return protected_cont(cont)


def start_child(computation: Async[_T], ms: int | TimeSpan | None = None) -> Async[Async[_T]]:
    if ms is not None:

        def binder(results: list[_T | _U]) -> Async[_T]:
            # TODO: the type error is correct and the implementation looks suspicious
            # since we use parallel2 which will wait for both computations to finish
            return protected_return(results[0])

        computation_with_timeout: Async[_T] = protected_bind(parallel2(computation, throw_after(ms)), binder)

        return start_child(computation_with_timeout)

    task = start_as_task(computation)

    def cont(ctx: IAsyncContext[Async[_T]]) -> None:
        protected_return(await_task(task))(ctx)

    return protected_cont(cont)


def start_immediate(
    computation: Async[Any],
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Start computation immediately.

    Runs an asynchronous computation, starting immediately on the
    current operating system thread
    """
    start_with_continuations(computation, cancellation_token=cancellation_token)


_executor: ThreadPoolExecutor | None = None


def start(
    computation: Callable[[IAsyncContext[Any]], None],
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Starts the asynchronous computation.

    Starts the asynchronous computation in the thread pool. Do not await
    its result.

    If no cancellation token is provided then the default cancellation
    token is used."""
    global _executor

    def worker() -> None:
        start_immediate(computation, cancellation_token)

    if not _executor:
        _executor = ThreadPoolExecutor(max_workers=16)
    _executor.submit(worker)
    return


def run_synchronously(
    computation: Async[_T],
    cancellation_token: CancellationToken | None = None,
) -> _T | None:
    """Run computation synchronously.

    Runs an asynchronous computation and awaits its result on the
    calling thread. Propagates an exception should the computation yield
    one. This call is blocking.
    """

    async def runner() -> _T:
        return await start_as_task(computation, cancellation_token=cancellation_token)

    return asyncio.run(runner())


__all__ = [
    "await_task",
    "cancel",
    "cancel_after",
    "cancellation_token",
    "catch_async",
    "create_cancellation_token",
    "delay",
    "from_continuations",
    "ignore",
    "is_cancellation_requested",
    "parallel",
    "sequential",
    "sleep",
    "start",
    "start_as_task",
    "start_child",
    "start_immediate",
    "start_with_continuations",
]
