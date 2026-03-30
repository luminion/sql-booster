package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class BoosterMapperUtilsTest {

    private List<TableResolver> originalResolvers;

    @Before
    public void setUp() {
        originalResolvers = new ArrayList<>(TableMetaRegistry.checkoutTableResolver());
        for (TableResolver resolver : originalResolvers) {
            TableMetaRegistry.removeTableResolver(resolver);
        }
    }

    @After
    public void tearDown() {
        for (TableResolver resolver : TableMetaRegistry.checkoutTableResolver()) {
            TableMetaRegistry.removeTableResolver(resolver);
        }
        for (TableResolver resolver : originalResolvers) {
            TableMetaRegistry.addTableResolver(resolver);
        }
    }

    @Test
    public void shouldFallbackToSimpleClassNameWhenNoTableMetadataExists() {
        String sql = BoosterMapperUtils.getSqlContent(SampleOrderEntity.class);

        Assert.assertTrue(sql.contains("FROM"));
        Assert.assertTrue(sql.contains("sample_order_entity a"));
        Assert.assertFalse(sql.contains(SampleOrderEntity.class.getName()));
    }

    static class SampleOrderEntity {
    }
}