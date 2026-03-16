use anyhow::{Context as _, anyhow, bail};

#[derive(Default)]
pub(crate) struct BinaryWriter {
    bytes: Vec<u8>,
}

impl BinaryWriter {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }

    pub(crate) fn bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }

    pub(crate) fn u8(&mut self, value: u8) {
        self.bytes.push(value);
    }

    pub(crate) fn u16(&mut self, value: u16) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    pub(crate) fn u32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    pub(crate) fn u64(&mut self, value: u64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    pub(crate) fn i64(&mut self, value: i64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    pub(crate) fn bool(&mut self, value: bool) {
        self.u8(u8::from(value));
    }

    pub(crate) fn len(&mut self, len: usize) {
        self.u32(u32::try_from(len).expect("length should fit into u32"));
    }

    pub(crate) fn string(&mut self, value: &str) {
        self.len(value.len());
        self.bytes(value.as_bytes());
    }

    pub(crate) fn option<T>(&mut self, value: Option<T>, encode: impl FnOnce(&mut Self, T)) {
        match value {
            Some(value) => {
                self.bool(true);
                encode(self, value);
            }
            None => self.bool(false),
        }
    }
}

pub(crate) struct BinaryReader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> BinaryReader<'a> {
    pub(crate) fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    pub(crate) fn offset(&self) -> usize {
        self.offset
    }

    pub(crate) fn finished(&self) -> bool {
        self.offset == self.bytes.len()
    }

    pub(crate) fn ensure_finished(&self) -> anyhow::Result<()> {
        if self.finished() {
            return Ok(());
        }
        bail!(
            "binary codec did not consume the full buffer: {} trailing byte(s)",
            self.bytes.len() - self.offset
        )
    }

    pub(crate) fn bytes(&mut self, len: usize) -> anyhow::Result<&'a [u8]> {
        let end = self.offset.checked_add(len).ok_or_else(|| anyhow!("binary read overflowed"))?;
        let slice =
            self.bytes.get(self.offset..end).ok_or_else(|| anyhow!("unexpected end of buffer"))?;
        self.offset = end;
        Ok(slice)
    }

    pub(crate) fn fixed<const N: usize>(&mut self) -> anyhow::Result<[u8; N]> {
        let bytes = self.bytes(N)?;
        bytes.try_into().map_err(|_error| anyhow!("expected exactly {N} byte(s)"))
    }

    pub(crate) fn u8(&mut self) -> anyhow::Result<u8> {
        Ok(self.fixed::<1>()?[0])
    }

    pub(crate) fn u16(&mut self) -> anyhow::Result<u16> {
        Ok(u16::from_le_bytes(self.fixed::<2>()?))
    }

    pub(crate) fn u32(&mut self) -> anyhow::Result<u32> {
        Ok(u32::from_le_bytes(self.fixed::<4>()?))
    }

    pub(crate) fn u64(&mut self) -> anyhow::Result<u64> {
        Ok(u64::from_le_bytes(self.fixed::<8>()?))
    }

    pub(crate) fn i64(&mut self) -> anyhow::Result<i64> {
        Ok(i64::from_le_bytes(self.fixed::<8>()?))
    }

    pub(crate) fn bool(&mut self) -> anyhow::Result<bool> {
        match self.u8()? {
            0 => Ok(false),
            1 => Ok(true),
            value => bail!("invalid bool tag `{value}` in binary codec"),
        }
    }

    pub(crate) fn len(&mut self) -> anyhow::Result<usize> {
        usize::try_from(self.u32()?).context("length did not fit into usize")
    }

    pub(crate) fn string(&mut self) -> anyhow::Result<String> {
        let len = self.len()?;
        let bytes = self.bytes(len)?;
        String::from_utf8(bytes.to_vec()).context("binary codec string was not valid UTF-8")
    }

    pub(crate) fn option<T>(
        &mut self,
        decode: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<Option<T>> {
        if self.bool()? { decode(self).map(Some) } else { Ok(None) }
    }
}
