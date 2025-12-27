use codspeed_criterion_compat::{
    Criterion, Throughput, black_box, criterion_group, criterion_main,
};

static SOURCE: &str = "
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
foobar(val fun loop while val loop while val) { + ++ = == === => }
";

static IDENTIFIERS: &str =
    "It was the year when they finally immanentized the Eschaton It was the year when they \
     finally immanentized the Eschaton It was the year when they finally immanentized the \
     Eschaton It was the year when they finally immanentized the Eschaton It was the year when \
     they finally immanentized the Eschaton It was the year when they finally immanentized the \
     Eschaton It was the year when they finally immanentized the Eschaton It was the year when \
     they finally immanentized the Eschaton It was the year when they finally immanentized the \
     Eschaton It was the year when they finally immanentized the Eschaton It was the year when \
     they finally immanentized the Eschaton It was the year when they finally immanentized the \
     Eschaton It was the year when they finally immanentized the Eschaton";

static CANDIDATES: [(&str, &str); 2] =
    [("identifiers", IDENTIFIERS), ("keywords_operators_and_punctators", SOURCE)];

fn iterate(s: &str) {
    use mitki_tokenizer::Tokenizer;

    let tokenizer = Tokenizer::new(s);
    black_box(tokenizer);
}

fn bench_iterate(c: &mut Criterion) {
    let mut group = c.benchmark_group("iterate");

    for (name, source) in CANDIDATES {
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_with_input(name, &source, |b, &s| b.iter(|| iterate(s)));
    }
}

criterion_group!(benches, bench_iterate);
criterion_main!(benches);
