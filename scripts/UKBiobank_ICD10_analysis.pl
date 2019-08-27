use strict;
use warnings;

my $code_groupings = "../data_files/ICD10_categories.txt";
my $subcode_exclusions = "../data_files/ICD10_subcat_exclusions.txt";
my $data = "Formatted_phenotype_data.txt"; # matrix with a sample per row with genotype ("pLoF" or "None") as second column and "Y" or "N" for whether should be included (unrelated European) in third column

# save exclusions
my %exclusions;

open(ICDEX, $subcode_exclusions)||die "Cannot open $subcode_exclusions";
while(<ICDEX>)
{
	chomp;
	$exclusions{$_}=$1;
}

# save codes
my %codes;

open(ICD, $code_groupings)||die "Cannot open $code_groupings";
my $head = <ICD>;
while(<ICD>)
{
	chomp;
	my ($code, $desc, $group, $include) = split /\t/, $_;
	if ($include eq 'Y')
	{
		$codes{$code}=$group;
	}
}

# read in phenotype file
my @fields = ("28","29","39"); # 0 indexed columns containing fields ‘41270 Diagnoses - ICD10’, ‘40001 Underlying (primary) cause of death: ICD10’ and ‘40002 Contributory (secondary) causes of death: ICD10’

my $count_kidney_carrier=0;
my $count_lung_carrier=0;
my $count_liver_carrier=0;

my $count_kidney_none=0;
my $count_lung_none=0;
my $count_liver_none=0;

my $count_any_carrier=0;
my $count_any_none=0;

open(PHEN, $data)||die "Cannot open $data";
my $header = <PHEN>;
while(<PHEN>)
{
	chomp;
	my @line = split /\t/, $_;

	my $liver_flag=0;
	my $lung_flag=0;
	my $kidney_flag=0;

	if ($line[2] eq 'Y')
	{
		foreach my $field (@fields)
		{
			my $entry = $line[$field];
			if ($entry eq 'NA'){}
			else
			{
				my @split_codes = split /,/, $entry;
				foreach my $icd_code (@split_codes)
				{
					if ($icd_code =~ /([A-Z][0-9][0-9])/)
					{
						my $code_start = $1;
						if ($icd_code =~ /\./)
						{
							print "ERROR $icd_code has a dot\n";
						}
						elsif ($icd_code =~ /([A-Z][0-9]+)/)
						{
							my $full_code = $1;
							if (exists $exclusions{$full_code})
							{
#								print "Excluded $full_code\n";
							}
							else
							{
								if (exists $codes{$code_start})
								{
									my $code_group = $codes{$code_start};
									if ($code_group eq 'Lung')
									{
										if ((($code_start eq 'J43')||($code_start eq 'J44')||($code_start eq 'J47'))&&(($entry =~ /Z7722/)||($entry =~ /P9681/)||($entry =~ /Z87891/)||($entry =~ /Z5731/)||($entry =~ /F17/)||($entry =~ /Z720/))){}
										else
										{
											$lung_flag=1;
											if ($line[1] eq 'pLoF')
											{
												print "$icd_code";
											}
										}
									}
									elsif ($code_group eq 'Liver')
									{
										$liver_flag=1;
										if ($line[1] eq 'pLoF')
										{
											print "$icd_code";
										}
									}
									elsif ($code_group eq 'Kidney')
									{
										$kidney_flag=1;
										if ($line[1] eq 'pLoF')
										{
											print "$icd_code";
										}
									}
								}
							}
						}
					}
				}
			}
		}
		if ($line[1] eq 'pLoF')
		{
			if ($lung_flag==1)
			{
				$count_lung_carrier++;
			}
			if ($kidney_flag==1)
			{
				$count_kidney_carrier++;
			}
			if ($liver_flag==1)
			{
				$count_liver_carrier++;
			}

			if (($lung_flag==1)||($kidney_flag==1)||($liver_flag==1))
			{
				$count_any_carrier++;
				print "- $line[0]\n";
			}
		}
		elsif ($line[1] eq 'None')
		{
			if ($lung_flag==1)
			{
				$count_lung_none++;
			}
			if ($kidney_flag==1)
			{
				$count_kidney_none++;
			}
			if ($liver_flag==1)
			{
				$count_liver_none++;
			}

			if (($lung_flag==1)||($kidney_flag==1)||($liver_flag==1))
			{
				$count_any_none++;
			}
		}
	}
}

print "Carriers:\n$count_lung_carrier lung\n$count_liver_carrier liver\n$count_kidney_carrier kidney\n";
print "Non-carriers:\n$count_lung_none lung\n$count_liver_none liver\n$count_kidney_none kidney\n";
print "Aggregate carriers affected: $count_any_carrier\nNon-carriers: $count_any_none\n";

