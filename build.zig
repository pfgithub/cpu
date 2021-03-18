const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    {
        const exe = b.addExecutable("cpu", "src/logic_emulator.zig");
        exe.setTarget(target);
        exe.setBuildMode(mode);
        {
            const pkgfile = "zig-cache/logic.zig";
            const sh_cmd = "./deno run -q src/cpu_logic.ts > " ++ pkgfile;
            const runner = b.addSystemCommand(&[_][]const u8{ "sh", "-c", sh_cmd });
            exe.step.dependOn(&runner.step);
            exe.addPackagePath("logic", pkgfile);
        }
        exe.install();

        const run_cmd = exe.run();
        run_cmd.step.dependOn(&exe.install_step.?.step);

        const run_step = b.step("run", "Run the CPU");
        run_step.dependOn(&run_cmd.step);
    }
}
