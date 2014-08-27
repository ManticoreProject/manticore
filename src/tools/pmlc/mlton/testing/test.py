#!/usr/bin/env python3

import subprocess
import re
import logging
import glob
import argparse
import os

from sys import argv

logging.basicConfig(level=logging.WARNING)
LOG = logging.getLogger(__name__)

class SMLFailure(Exception):
    pass

class SMLTimeout(Exception):
    pass

class SMLOutput:
    CM_FINISHED = "[New bindings added.]"
    EXN = r"uncaught exception (\w+)"
    ERROR = r"Error: (\w+)"
    SUCCEESS = "Completed generation of sxml"
    EXPECTED_FAILURE = "FAIL"

    def __init__(self, out_errs):
        self.out, self.errs = out_errs
        self.exn_re = re.compile(self.EXN)
        self.error_re = re.compile(self.ERROR)
        self.fail_re = re.compile(self.EXPECTED_FAILURE)

    def _match_result(self, maybe_re_results):
        return maybe_re_results.group(0) if maybe_re_results else None

    def _check_re(self, re_):
        return self._match_result(re_.search(self.out))

    def finished_compile(self):
        return (not self.errs) and (self.CM_FINISHED in self.out)

    def raised_exn(self):
        return self._check_re(self.exn_re)

    def raised_error(self):
        return self._check_re(self.error_re)

    def check_compile(self):
        failure = self.raised_exn() or self.raised_error()
        if failure:
            raise SMLFailure(failure)

    def succeeded(self):
        return self.SUCCEESS in self.out

    # Note that false positives are possible
    def failed_because(self, reason):
        LOG.debug("in output: {}".format(self.out))
        LOG.debug("Checking for reason: {}".format(reason))
        LOG.debug("Index: {}".format(self.out.find(reason)))
        LOG.debug("Failed correctly? {}".format(reason in self.out))
        return reason in self.out

    def short_output(self):
        return self.out.split("[New bindings added.]")[1]

class SMLProgram:
    SML_CMD = "sml"

    def __init__(self, cmd):
        self._proc = subprocess.Popen(self.SML_CMD,
                                      stdin=subprocess.PIPE,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE,
                                      universal_newlines=True)
        # Note that SML/NJ chokes up without universal_newlines=True
        self._cmd = cmd

    def get_output(self, timeout):
        return SMLOutput(self._proc.communicate(self._cmd,
                                                timeout=timeout))

    def terminate(self):
        self._proc.terminate()

class SMLFile:
    EXPECTED_FAILURE = r"FAIL (\[[^]]+\])?"

    def __init__(self, path):
        self.fail_re = re.compile(self.EXPECTED_FAILURE)
        with open(path, "r") as f:
            self._body = f.readlines()

    def should_fail(self):
        return bool(self.fail_re.search(self._body[0]))

    def should_fail_because(self):
        reason = self.fail_re.search(self._body[0])
        if reason:
            return (reason.group(1) or "").strip("[]")
        else:
            return ""

    def passes_on_output(self, output):
        fail_reason = self.should_fail_because()
        LOG.debug("Got fail reason: {}".format(fail_reason))
        if self.should_fail():
            return output.failed_because(self.should_fail_because())
        else:
            return output.succeeded()

class Terminal:
    RED = "\033[0;31m"
    GREEN = "\033[0;32m"
    ENDCOLOR = "\033[0;0m"

    def _color(self, to_color, start_color):
        return "{}{}{}".format(start_color, to_color, self.ENDCOLOR)

    def _print_result_for(self, result, path):
        print("{} for {}".format(result, path))

    def print_failure(self, path):
        self._print_result_for(self._color("TEST FAILED", self.RED),
                               path)

    def print_success(self, path):
        self._print_result_for(self._color("TEST PASSED", self.GREEN),
                               path)

class Directory:
    PRECOMPILE_TIMEOUT = 10
    PRECOMPILE_ATTEMPTS = 3
    SML_COMPILE = 'CM.make "../sources.cm";'

    RUN_TIMEOUT = 1.5
    SML_RUN_FMT = '''CM.make "../sources.cm";
    PMLFrontEnd.init();
    PMLFrontEnd.compilePML {{input=["{}"]}};'''

    DIRCHECK_FMT = "{}/*.pml"

    def __init__(self, test_dir):
        self.test_dir = test_dir
        self._precompile()
        self._num_passed = 0
        self._num_total = 0
        self._terminal = Terminal()

    def _precompile(self):
        for _ in range(self.PRECOMPILE_ATTEMPTS):
            if self._precompile_succeeded():
                return

        raise SMLFailure("Exceeded maximum number of attempts to compile.")

    def _precompile_succeeded(self):
        sml = SMLProgram(self.SML_COMPILE)
        output = None

        try:
            output = sml.get_output(self.PRECOMPILE_TIMEOUT)
        except SMLTimeout:
            sml.terminate()
            output = sml.get_output(None)

        LOG.debug("out: {}".format(output.out))

        output.check_compile()
        return output.finished_compile()

    def test(self):
        for path in sorted(glob.glob(self.DIRCHECK_FMT.format(self.test_dir))):
            self.test_file(path)

        print("{} failures in {} tests".format(self._num_total - self._num_passed,
                                               self._num_passed))

    def test_file(self, path):
        file_ = SMLFile(path)
        out = self._output_for_path(path)
        LOG.info("Output: {}".format(out.short_output()))
        self._log_result(file_.passes_on_output(out), path)

    def _output_for_path(self, path):
        LOG.debug("Running for path: {}".format(path))
        sml = SMLProgram(self._build_run_cmd(path))

        try:
            out = sml.get_output(self.RUN_TIMEOUT)
        except SMLTimeout:
            sml.terminate()
            out = sml.get_output(None)
            raise SMLFailure(out.raised_error())

        return out

    def _build_run_cmd(self, path):
        run_cmd = self.SML_RUN_FMT.format(path)
        LOG.debug("Running input: {}".format(run_cmd))
        return run_cmd

    def _log_result(self, result, path):
        LOG.debug("Result: {}, path: {}".format(result, path))
        self._num_total += 1
        if result:
            self._num_passed += 1
            self._terminal.print_success(path)
        else:
            self._terminal.print_failure(path)




if __name__ == '__main__':
    arg_parser = argparse.ArgumentParser(description="Run Manticore tests.")
    arg_parser.add_argument(
        "-d", "--directory",
        dest="dir_",
        action="store",
        default=None,
        help="Directory containing test files.")
    arg_parser.add_argument(
        "-f", "--file",
        dest="file_",
        action="store",
        default=None,
        help="Single file to test.")
    arg_parser.add_argument(
        "-v", "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="Enable verbose output.")

    args = arg_parser.parse_args()

    if args.verbose:
        LOG.setLevel(logging.INFO)

    if args.dir_:
        test_dir = Directory(args.dir_)
        test_dir.test()
    elif args.file_:
        # file_, dir_ = os.path.split(args.file_) *)
        test_dir = Directory(None)
        test_dir.test_file(args.file_)
