package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.support.BoosterTestFixtures;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class BoosterRegistryTest {

    private BoosterTestFixtures.TestBooster booster;

    @Before
    public void setUp() {
        BoosterTestFixtures.registerTestResolver();
        booster = new BoosterTestFixtures.TestBooster();
        BoosterRegistry.registerDefault("testBooster", booster);
    }

    @After
    public void tearDown() {
        BoosterTestFixtures.clearRegistries();
    }

    @Test
    public void entityInterfaceShouldResolveDefaultBooster() {
        BoosterTestFixtures.TestEntity entity = new BoosterTestFixtures.TestEntity();

        BoosterTestFixtures.TestView view = entity.booster().voById(1L);
        assertEquals("tom", view.getName());
    }

    @Test
    public void staticRegistryLambdaShouldResolveDefaultResultType() {
        List<BoosterTestFixtures.TestView> views = BoosterRegistry.lambda(BoosterTestFixtures.TestEntity.class,
                BoosterTestFixtures.TestView.class).list();

        assertEquals(1, views.size());
        assertEquals("tom", views.get(0).getName());
    }

    @Test
    public void lambdaBoosterShouldStillSupportExplicitConversion() {
        List<BoosterTestFixtures.TestDto> dtos = BoosterRegistry.lambda(BoosterTestFixtures.TestEntity.class,
                BoosterTestFixtures.TestView.class).list(BoosterTestFixtures.TestDto.class);

        assertEquals(1, dtos.size());
        assertEquals("tom", dtos.get(0).getName());
        assertEquals(Integer.valueOf(1), dtos.get(0).getState());
    }

    @Test
    public void entityLambdaBoosterShouldUseRegisteredDefaultBooster() {
        BoosterTestFixtures.TestEntity entity = new BoosterTestFixtures.TestEntity();
        entity.lambdaBooster().eq(BoosterTestFixtures.TestEntity::getName, "tom").list();

        SqlContext<BoosterTestFixtures.TestEntity> lastContext = booster.getLastContext();
        assertNotNull(lastContext);
        assertEquals(1, lastContext.getConditions().size());
    }

    @Test(expected = IllegalStateException.class)
    public void duplicateDefaultBoosterRegistrationShouldFailFast() {
        BoosterRegistry.registerDefault("anotherTestBooster", new BoosterTestFixtures.TestBooster());
    }
}