class Concrete < Formula
  desc "Systems programming language without garbage collection"
  homepage "https://github.com/lambdaclass/concrete"
  license "Apache-2.0"
  head "https://github.com/lambdaclass/concrete.git", branch: "main"

  allow_network_access! :build

  depends_on "elan-init" => :build
  depends_on "git" => :build
  depends_on "llvm"
  depends_on "zstd"

  def install
    ENV["ELAN_HOME"] = buildpath/".elan"
    ENV.prepend_path "PATH", Formula["elan-init"].bin
    ENV.prepend_path "PATH", (buildpath/".elan/bin").to_s
    system "elan-init", "-y", "--no-modify-path", "--default-toolchain", "none"

    toolchain = (buildpath/"lean-toolchain").read.strip
    system "elan", "toolchain", "install", toolchain
    system "elan", "default", toolchain

    ENV.prepend_path "PATH", Formula["llvm"].opt_bin
    if OS.mac?
      ENV["LIBRARY_PATH"] = [
        Formula["zstd"].opt_lib,
        ENV["LIBRARY_PATH"],
      ].compact.join(":")
    else
      ENV.append "LDFLAGS", "-L#{Formula["zstd"].opt_lib}"
    end

    system "lake", "build", "concrete"
    bin.install ".lake/build/bin/concrete"
  end

  test do
    assert_match "concrete 0.1.0", shell_output("#{bin}/concrete --version")
    system bin/"concrete", "examples/snippets/hello_world.con", "-o", testpath/"hello"
    assert_match "hello", shell_output(testpath/"hello")
  end
end
