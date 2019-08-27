use strict;
use warnings;

my $pheno_codes = "../data_files/Self_reported_phenotypes.txt";
my $data = "Formatted_phenotype_data.txt"; # matrix with a sample per row with genotype ("pLoF" or "None") as second column and "Y" or "N" for whether should be included (unrelated European) in third column (also here have sex in fourth column and age at recruitment and death in fifth and sixth columns respectively)

my $cancer_col=10; # 0 indexed column containing field 20001 Cancer code, self-reported
my $non-cancer_col=11; # 0 indexed column containing field 20002 Non-cancer illness code, self-reported

# read in phenotype codes
my @ordered_pheno_groups;
my %pheno_groups_primary;
my %pheno_groups_secondary;
my %found_pheno;

open(PHEN, $pheno_codes)||die "Cannot open $pheno_codes";
while(<PHEN>)
{
	chomp;
	my ($coding, $desc, $prim_group, $sec_group) = split /\t/, $_;

	if ($prim_group)
	{
		if (($prim_group eq 'exclude')||($prim_group eq 'Exclude')){}
		elsif ($coding =~ /([0-9]+)/)
		{
			my $code = $1;
			$pheno_groups_primary{$code}=$prim_group;
			if (exists $found_pheno{$prim_group}){}
			else
			{
				push(@ordered_pheno_groups, $prim_group);
				$found_pheno{$prim_group}=1;
			}

			if ($sec_group)
			{
				$pheno_groups_secondary{$code}=$sec_group;
				if (exists $found_pheno{$sec_group}){}
				else
				{
					push(@ordered_pheno_groups, $sec_group);
					$found_pheno{$sec_group}=1;
				}
			}
		}
	}
}

my $num_pheno_groups = @ordered_pheno_groups;
print "Found $num_pheno_groups phenotype groupings\n";

# print out header line
open(OUT, ">Grouped_phenotype_data.txt");
print OUT "sample\tgenotype\teur\tSex\tAge_at_recruitment\tAge_at_death\tCancer\tParkinsons";

foreach my $group (@ordered_pheno_groups)
{
	print OUT "\t$group";
}
print OUT "\n";

# read through samples
open(DAT, $data)||die "Cannot open $data";
my $head=<DAT>;
while(<DAT>)
{
	chomp;
	my %this_samp;
	my @line = split /\t/, $_;

	my $cancer = "N";
	if ($line[$cancer_col] eq 'NA'){}
	else
	{
		$cancer = "Y";
	}

	my $PD = "N";

	my $other_phenos= $line[$non-cancer_col];
	my @each_pheno = split /,/, $other_phenos;
	foreach my $phenotype (@each_pheno)
	{
		if (exists $pheno_groups_primary{$phenotype})
		{
			$this_samp{$pheno_groups_primary{$phenotype}}=1;
		}
		if (exists $pheno_groups_secondary{$phenotype})
		{
			$this_samp{$pheno_groups_secondary{$phenotype}}=1;
		}
		if ($phenotype eq 'NA'){}
		elsif ($phenotype==1262) # specifically pull if have self-reported Parkinson's disease
		{
			$PD = "Y";
		}
	}

	print OUT "$line[0]\t$line[1]\t$line[2]\t$line[3]\t$line[4]\t$line[5]\t$cancer\t$PD";

	foreach my $phen_group (@ordered_pheno_groups)
	{
		if (exists $this_samp{$phen_group})
		{
			print OUT "\tY";
		}
		else
		{
			print OUT "\tN";
		}
	}

	print OUT "\n";

	undef %this_samp;
}

