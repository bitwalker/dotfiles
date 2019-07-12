function run_prompt_benchmark -d "Runs a benchmark/profiler against the fish_prompt function"
    mkdir -p "$XDG_CACHE_HOME/fish"
    set -l trace_file "$XDG_CACHE_HOME/fish/prompt.trace"

    # Run trace
    fish -p "$trace_file" -c "fish_prompt"

    # Print trace
    printf "Top 20 Time Consumers\nTime Sum Command\n\n"
    cat "$trace_file" | sort -g | tail -n 20

    # Clean up trace file
    rm -f "$trace_file" 2>/dev/null
end
