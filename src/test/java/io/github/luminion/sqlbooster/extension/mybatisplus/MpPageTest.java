package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.model.BPage;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

public class MpPageTest {

    @Test
    public void shouldExposePageDataAndConvertRecords() {
        PageDTO<SourceRecord> pageDTO = new PageDTO<>(2L, 3L);
        pageDTO.setTotal(5L);
        pageDTO.setRecords(Arrays.asList(new SourceRecord("A"), new SourceRecord("B")));

        MpPage<SourceRecord> page = new MpPage<SourceRecord>(pageDTO);

        Assert.assertEquals(2L, page.getCurrent());
        Assert.assertEquals(3L, page.getSize());
        Assert.assertEquals(5L, page.getTotal());
        Assert.assertEquals(2L, page.getPages());

        BPage<TargetRecord> converted = page.convertRecords(TargetRecord.class);
        Assert.assertEquals(2, converted.getRecords().size());
        Assert.assertEquals("A", converted.getRecords().get(0).getName());
        Assert.assertEquals("B", converted.getRecords().get(1).getName());
    }

    public static final class SourceRecord {
        private String name;

        public SourceRecord() {
        }

        private SourceRecord(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public static final class TargetRecord {
        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }
}
