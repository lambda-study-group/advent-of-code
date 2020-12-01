package br.com.souenzzo.advent_of_code_2020;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class IOStream {
    static Stream<String> fileLines(String name) throws FileNotFoundException {
        var f = new File(name);
        return new BufferedReader(new FileReader(f)).lines();
    }
}

public class aoc_1 {
    static Stream<Long> solutions(Stream<String> input) {
        // O(2n)
        var allInts = input.map(Long::parseLong)
                .collect(Collectors.toSet());
        return allInts.stream().flatMap(el -> {
            var diff = 2020 - el;
            return allInts.contains(diff)
                    ? Stream.of(diff * el)
                    : Stream.empty();
        }).distinct();
    }

    static Stream<Long> solutions_extra(Stream<String> input) {
        // O(n³)
        var allInts = input.map(Long::parseLong)
                .collect(Collectors.toSet());
        return allInts.stream()
                .flatMap(a -> allInts.stream()
                        .flatMap(b -> allInts.stream()
                                .flatMap(c -> (2020 == (a + b + c))
                                        ? Stream.of(a * b * c)
                                        : Stream.empty()))).distinct();
    }

    public static void main(String[] argv) throws IOException {
        System.out.println("Frist problem: 100419");
        solutions(IOStream.fileLines("resources/day/1/input"))
                .forEach(System.out::println);
        System.out.println("Extra problem: 265253940");
        solutions_extra(IOStream.fileLines("resources/day/1/input"))
                .forEach(System.out::println);
    }
}
