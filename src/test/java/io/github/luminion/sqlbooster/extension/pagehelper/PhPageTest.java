package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collections;
import java.util.List;
import java.util.Arrays;

public class PhPageTest {

    @Test
    public void shouldRejectPageArgumentsOutsidePageHelperRange() {
        PhMapper<Object, Object> mapper = new PhMapper<Object, Object>() {
            @Override
            public List<Object> selectByXml(SqlContext<Object> sqlContext, Object page) {
                return Collections.emptyList();
            }
        };

        try {
            mapper.voPage(new SqlContext<Object>(), (long) Integer.MAX_VALUE + 1L, 1L);
            Assert.fail("Expected oversized page number to be rejected.");
        } catch (IllegalArgumentException expected) {
        }
    }
    @Test
    public void shouldExposePageDataAndConvertRecords() {
        Page<SourceRecord> page = new Page<SourceRecord>(2, 3);
        page.setTotal(5L);
        page.addAll(Arrays.asList(new SourceRecord("A"), new SourceRecord("B")));

        PageInfo<SourceRecord> pageInfo = page.toPageInfo();
        PhPage<SourceRecord> phPage = new PhPage<SourceRecord>(pageInfo);

        Assert.assertEquals(2L, phPage.getCurrent());
        Assert.assertEquals(3L, phPage.getSize());
        Assert.assertEquals(5L, phPage.getTotal());
        Assert.assertEquals(2L, phPage.getPages());

        BPage<TargetRecord> converted = phPage.convertRecords(TargetRecord.class);
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
