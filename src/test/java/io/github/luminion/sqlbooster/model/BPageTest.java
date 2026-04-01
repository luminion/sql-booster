package io.github.luminion.sqlbooster.model;

import org.junit.Assert;
import org.junit.Test;

import java.util.Collections;
import java.util.List;

public class BPageTest {

    @Test
    public void shouldReturnZeroPagesWhenPageSizeIsZero() {
        TestPage<String> page = new TestPage<String>(Collections.singletonList("A"), 5L, 1L, 0L);

        Assert.assertEquals(0L, page.getPages());
    }

    @Test
    public void shouldRoundUpPagesWhenTotalIsNotDivisibleBySize() {
        TestPage<String> page = new TestPage<String>(Collections.singletonList("A"), 5L, 1L, 2L);

        Assert.assertEquals(3L, page.getPages());
    }

    private static final class TestPage<T> implements BPage<T> {
        private static final long serialVersionUID = 1L;

        private final List<T> records;
        private final long total;
        private final long current;
        private final long size;

        private TestPage(List<T> records, long total, long current, long size) {
            this.records = records;
            this.total = total;
            this.current = current;
            this.size = size;
        }

        @Override
        public List<T> getRecords() {
            return records;
        }

        @Override
        public long getTotal() {
            return total;
        }

        @Override
        public long getCurrent() {
            return current;
        }

        @Override
        public long getSize() {
            return size;
        }

        @Override
        public <R> BPage<R> convertRecords(Class<R> clazz) {
            return new TestPage<R>(Collections.<R>emptyList(), total, current, size);
        }
    }
}
