##--------------Anova-------------------##

<<<<<<<<<<<<<<<exploratory analysis>>>>>>>>>>>>>>>>>>
  
  # Using late_shipments, group by shipment mode, and calculate the mean and std dev of pack price
  late_shipments %>% 
  group_by(shipment_mode)%>%
  summarize(xbar_pack_price = mean(pack_price),
            s_pack_price = sd(pack_price))

# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
late_shipments %>% 
  ggplot(aes(shipment_mode, pack_price))+
  geom_boxplot()+
  coord_flip()

The box plots made it look like the distribution of pack price 
was different for each of the three shipment modes. However, 
it didn't tell us whether the mean pack price was different 
in each category. To determine that, we can use an ANOVA test. 
The null and alternative hypotheses can be written as follows.

H1: Pack prices for every category of shipment mode are the same. 
H2: Pack prices for some categories of shipment mode are different.

We'll set a significance level of 0.1.

<<<<<<<<<<<<<<<<<<ANOVA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Run a linear regression of pack price vs. shipment mode 
  mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, 
                                        data = late_shipments)

# See the results
summary(mdl_pack_price_vs_shipment_mode)

# Perform ANOVA on the regression model
anova(mdl_pack_price_vs_shipment_mode)

##---------------Pairwise.t.test---------------##

The ANOVA test didn't tell us which categories of shipment mode had 
significant differences in pack prices. To pinpoint which categories 
had differences, we could instead use pairwise t-tests.

# Perform pairwise t-tests on pack price, grouped by shipment mode, no p-value adjustment
test_results <- pairwise.t.test(late_shipments$pack_price, 
late_shipments$shipment_mode, p.adjust.method = "none")

# Modify the pairwise t-tests to use Bonferroni p-value adjustment
test_results <- pairwise.t.test(
  late_shipments$pack_price,
  late_shipments$shipment_mode,
  p.adjust.method = "bonferroni"
)
